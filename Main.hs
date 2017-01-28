{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Main
  ( module Main
  , module Types
  , module Evaluable
  , module Utils
  ) where

import           Control.Applicative
import           Control.Arrow                     ((***))
import           Control.Monad.Except
import qualified Control.Monad.Fail                as Fail
import           Data.Bifunctor
import           Data.Either
import           Data.Foldable                     (asum)
import           Data.Maybe
import           Data.Semigroup

import           Data.Kind
import           GHC.Exts                          (IsList (..))
import           GHC.Generics                      (Generic)
import qualified GHC.Stack                         as Stack
import           GHC.TypeLits

import           Data.List                         (maximumBy, sortBy)
import           Data.Ord                          (comparing)

import           Control.Lens                      hiding
                 ((.>), (<.), (<|), (|>))
import           Data.Aeson.Lens

import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V

import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS

import qualified Codec.Compression.GZip            as GZ
import qualified Data.ByteString.Base64.Lazy       as Base64

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map

import qualified Data.HashMap.Lazy                 as LHM

import           Data.Aeson
                 (FromJSON (..), ToJSON (..), Value (..), (.:))
import qualified Data.Aeson                        as A
import qualified Data.Aeson.BetterErrors           as ABE
import qualified Data.Aeson.Encode.Pretty          as A
import qualified Data.Aeson.Types                  as A

import           Data.Version

import qualified Language.PureScript.AST.Literals  as PS
import qualified Language.PureScript.AST.SourcePos as PS
import qualified Language.PureScript.Comments      as PS
import qualified Language.PureScript.Names         as PS
import qualified Language.PureScript.Types         as PS

import           Language.PureScript.AST.Literals  (Literal (..))
import           Language.PureScript.Names

import           Evaluable
import           Types
import           Utils

data CustomJSONError = CustomJSONError Text
                     deriving (Eq, Show)

type JParse a = A.Parser a
-- type JParse a = ABE.Parse CustomJSONError a
type JSON = Value

type AnnParser a = JSON -> JParse (a, JSON)

type HCS = Stack.HasCallStack
type StrLike str = (Monoid str, IsList str, Item str ~ Char)

failure :: forall str m a. (HCS, StrLike str, Monad m)
        => [str] -> m a
failure xs = [ stackTrace
             , "\n\n"
             , toList $ mconcat xs
             ] |> mconcat |> fail
  where
    stackTrace = Stack.prettyCallStack Stack.callStack

failureT :: (Monad m) => [Text] -> m a
failureT = failure

defAnnP :: HCS => AnnParser ()
defAnnP v = pure ((), v)

newAnnP :: HCS => AnnParser [JSON]
newAnnP (Object o) = do ann <- o .: "ann"
                        val <- o .: "val"
                        pure ([ann], val)
newAnnP _          = empty

runInsideAP :: HCS => AnnParser a -> (a -> JSON -> JParse b) -> JSON -> JParse b
runInsideAP annP cb v = annP v >>= uncurry cb

runInsideAP' :: HCS => AnnParser a -> JSON -> (a -> JSON -> JParse b) -> JParse b
runInsideAP' annP v cb = annP v >>= uncurry cb

backtrace :: HCS => Text -> (JSON -> JParse a) -> JSON -> JParse a
backtrace msg f v = f v -- <|> failure [msg]

choice :: HCS => (Alternative f) => [f a] -> f a
choice = asum

wrapP :: HCS => (FromJSON a) => (a -> JParse b) -> JSON -> JParse b
wrapP f v = parseJSON v >>= f

instance Foldable Literal where
  foldMap f (ArrayLiteral  xs) = foldMap f xs
  foldMap f (ObjectLiteral xs) = foldMap (snd .> f) xs
  foldMap _ _                  = mempty

instance Traversable Literal where
  sequenceA = helper
    where
      helper (NumericLiteral n) = pure $ NumericLiteral n
      helper (StringLiteral  s) = pure $ StringLiteral s
      helper (CharLiteral    c) = pure $ CharLiteral c
      helper (BooleanLiteral b) = pure $ BooleanLiteral b
      helper (ArrayLiteral  xs) = ArrayLiteral  <$> sequenceA xs
      helper (ObjectLiteral ps) = ObjectLiteral <$> sequenceA (map sequenceA ps)

class (Functor f) => Annotated f where
  extractAnn :: HCS => f a -> a
  modifyAnn  :: HCS => (a -> a) -> f a -> f a

type PName  (a :: ProperNameType) = ProperName a
type QPName (a :: ProperNameType) = Qualified (ProperName a)
type QIdent     = Qualified Ident
type SourceSpan = PS.SourceSpan
type Comment    = PS.Comment

type Ann = (Maybe SourceSpan, [Comment], Maybe PS.Type, Maybe Meta)

nullAnn :: HCS => Ann
nullAnn = (Nothing, [], Nothing, Nothing)

removeComments :: HCS => Ann -> Ann
removeComments (mss, _, mt, mm) = (mss, [], mt, mm)

data Binder a
  = NullBinder    { _binderAnn :: a }
  | LiteralBinder { _binderAnn     :: a
                  , _binderLiteral :: Literal (Binder a) }
  | VarBinder     { _binderAnn   :: a
                  , _binderIdent :: Ident }
  | CtorBinder    { _binderAnn      :: a
                  , _binderCtorType :: QPName 'TypeName
                  , _binderCtorName :: QPName 'ConstructorName
                  , _binderCtorArgs :: [Binder a] }
  | NamedBinder   { _binderAnn    :: a
                  , _binderName   :: Ident
                  , _binderBinder :: Binder a }
  deriving (Show, Functor, Foldable, Traversable, Generic)

maybeToM :: HCS => (Monad m) => String -> Maybe a -> m a
maybeToM err = maybe (failure [err]) pure

parseTagList :: HCS => [(Text, JSON -> JParse a)] -> JSON -> JParse a
parseTagList m = backtrace "parseTagList"
                 $ \val -> do (k, v) <- helper val
                              cb <- lookupM k
                              cb v <|> failure3 k
  where
    lookupM k = lookup k m |> maybeToM (failure1 k)
    helper (String k)                               = pure (k, Null)
    helper (Array (uncons -> Just (String k, [v]))) = pure (k, v)
    helper (Array (uncons -> Just (String k, v)))   = pure (k, Array v)
    helper owise                                    = failure2 owise
    failure1 x = failure [ "parseTagList: failed to look up key: ", show x ]
    failure2 x = failure [ "parseTagList: error code 2: ",          show x ]
    failure3 x = failure [ "parseTagList: callback failed for ",    show x ]

parseModuleName :: HCS => JSON -> JParse ModuleName
parseModuleName = backtrace "parseModuleName"
                  $ parseJSON .> fmap moduleNameFromString

parseIdent :: HCS => JSON -> JParse Ident
parseIdent = backtrace "parseIdent"
             $ parseJSON .> fmap Ident

parseBind :: forall ann. HCS => AnnParser ann -> JSON -> JParse (Bind ann)
parseBind annP = backtrace "parseBind"
                 $ A.withObject "parseBind: not an object" (LHM.toList .> helper)
  where
    helper [(n, v)] = runInsideAP' annP v
                      $ \a -> parseExpr annP >=> NonRec a (Ident n) .> pure
    helper xs       = Rec <$> mapM mkRecBinding xs
    mkRecBinding :: (Text, JSON) -> JParse ((ann, Ident), Expr ann)
    mkRecBinding (k, v) = runInsideAP' annP v
                          $ \a -> parseExpr annP
                                  >=> (\e -> pure ((a, Ident k), e))

parseProperName :: HCS => JSON -> JParse (ProperName a)
parseProperName = backtrace "parseProperName" (parseJSON .> fmap ProperName)

parseQualified :: forall a. HCS => (Text -> JParse a) -> JSON -> JParse (Qualified a)
parseQualified p = backtrace "parseQualified" go
  where
    go :: JSON -> JParse (Qualified a)
    go (String s) = qualifiedP s <|> unqualifiedP s
    go owise      = failure ["parseQualified: not a string: ", show owise]
    qualifiedP, unqualifiedP :: Text -> JParse (Qualified a)
    qualifiedP s = case unsnoc $ T.split (== '.') s
                   of Just (xs, x) -> let mn = mkModName xs
                                      in Qualified (Just mn) <$> p x
                      Nothing      -> failureT
                                      ["parseQualified: no instance of '.'"]
    unqualifiedP s = Qualified Nothing <$> p s
    mkModName = moduleNameFromString . T.intercalate "."

parseLiteral :: HCS => (JSON -> JParse a) -> JSON -> JParse (Literal a)
parseLiteral cb = backtrace "parseLiteral"
                  $ parseTagList dispatch >=> fmap cb .> sequenceA
  where
    dispatch :: HCS => [(Text, JSON -> JParse (Literal JSON))]
    dispatch = let go f = parseJSON .> fmap f
               in [ ("IntLiteral",     go (Left  .> NumericLiteral))
                  , ("NumberLiteral",  go (Right .> NumericLiteral))
                  , ("StringLiteral",  go StringLiteral)
                  , ("CharLiteral",    go CharLiteral)
                  , ("BooleanLiteral", go BooleanLiteral)
                  , ("ArrayLiteral",   go ArrayLiteral)
                  , ("ObjectLiteral",  go (LHM.toList .> ObjectLiteral)) ]

parseBinder :: forall ann. HCS => AnnParser ann -> JSON -> JParse (Binder ann)
parseBinder annP = backtrace "parseBinder"
                   $ runInsideAP annP
                   $ \a -> parseTagList [ ("NullBinder",        nullP  a)
                                        , ("LiteralBinder",     litP   a)
                                        , ("VarBinder",         varP   a)
                                        , ("ConstructorBinder", ctorP  a)
                                        , ("NamedBinder",       namedP a) ]
  where
    nullP, litP, varP, ctorP, namedP :: ann -> JSON -> JParse (Binder ann)
    nullP   a = const $ pure $ NullBinder a
    litP    a = parseLiteral binderP .> fmap (LiteralBinder a)
    varP    a = wrapP $ \x             -> VarBinder a <$> parseIdent x
    ctorP   a = wrapP $ \(vt, vn, vbs) -> CtorBinder a
                                          <$> parseQPName vt
                                          <*> parseQPName vn
                                          <*> parseBinderList vbs
    namedP  a = wrapP $ \(vi, vb)      -> NamedBinder a
                                          <$> parseIdent vi
                                          <*> binderP vb

    binderP :: JSON -> JParse (Binder ann)
    binderP = parseBinder annP

    parseBinderList :: JSON -> JParse [Binder ann]
    parseBinderList = parseJSON >=> mapM binderP

    parseQPName :: JSON -> JParse (QPName x)
    parseQPName = parseQualified (ProperName .> pure)

parseCaseAlt :: forall ann. HCS => AnnParser ann -> JSON -> JParse (CaseAlternative ann)
parseCaseAlt annP = backtrace "parseCaseAlt"
                    $ wrapP $ \(vbs, vr) -> CaseAlternative
                                            <$> bindersP vbs
                                            <*> resultP vr
  where
    bindersP :: JSON -> JParse [Binder ann]
    bindersP = A.withArray "parseCaseAlt: bindersP: not an array"
               (V.mapM (parseBinder annP) .> fmap V.toList)
    resultP :: JSON -> JParse (Either [(Guard ann, Expr ann)] (Expr ann))
    resultP v = [ Left  <$> (parseJSON v >>= mapM guardP)
                , Right <$> parseExpr annP v
                ] |> choice
    guardP :: (JSON, JSON) -> JParse (Guard ann, Expr ann)
    guardP (g, e) = (,) <$> parseExpr annP g <*> parseExpr annP e

parseExpr :: forall ann. HCS => AnnParser ann -> JSON -> JParse (Expr ann)
parseExpr cb = backtrace "parseExpr"
               $ runInsideAP cb
               $ \a -> parseTagList
                       [ ("Literal",      backtrace "litP"  $ litP  a)
                       , ("Constructor",  backtrace "ctorP" $ ctorP a)
                       , ("Abs",          backtrace "absP"  $ absP  a)
                       , ("App",          backtrace "appP"  $ appP  a)
                       , ("Var",          backtrace "varP"  $ varP  a)
                       , ("Case",         backtrace "caseP" $ caseP a)
                       , ("Let",          backtrace "letP"  $ letP  a)
                       , ("Accessor",     backtrace "raP"   $ raP   a)
                       , ("ObjectUpdate", backtrace "ruP"   $ ruP   a) ]
  where
    litP, varP, ctorP, absP, appP, caseP, letP, raP, ruP
      :: HCS => ann -> JSON -> JParse (Expr ann)
    litP  a = parseLiteral exprP .> fmap (Lit a)
    varP  a = parseQualified (Ident .> pure) .> fmap (Var a)
    ctorP a = wrapP $ \(t, n, as) -> Ctor a
                                     <$> parseProperName t
                                     <*> parseProperName n
                                     <*> (parseJSON as >>= mapM parseIdent)
    absP  a = wrapP $ \(n, b)     -> Abs a <$> parseIdent n <*> exprP b
    appP  a = wrapP $ \(f, x)     -> App a <$> exprP f <*> exprP x
    caseP a = wrapP $ \(vs, as)   -> Case a
                                     <$> arrayP vs exprP
                                     <*> arrayP as (parseCaseAlt cb)
    letP  a = wrapP $ \(bs, e)    -> Let a
                                     <$> arrayP bs (parseBind cb)
                                     <*> exprP e
    raP   a = wrapP $ \(f, r)     -> RecAccess a
                                     <$> parseJSON f
                                     <*> exprP r
    ruP   a = wrapP $ \(r, fs)    -> RecUpdate a
                                     <$> exprP r
                                     <*> wrapP (tl .> mapM fieldP) fs
    tl = LHM.toList
    exprP :: HCS => JSON -> JParse (Expr ann)
    exprP = parseExpr cb
    fieldP :: HCS => (Text, JSON) -> JParse (Text, Expr ann)
    fieldP = second exprP .> sequenceA
    arrayP :: HCS => (FromJSON a) => JSON -> (a -> JParse b) -> JParse [b]
    arrayP v f = wrapP (mapM f) v

instance Annotated Binder where
  extractAnn = _binderAnn
  modifyAnn f b = b { _binderAnn = f $ _binderAnn b }

data Expr a
  = Lit       { _exprAnn     :: a
              , _exprLiteral :: Literal (Expr a) }
  | Var       { _exprAnn :: a
              , _exprVar :: QIdent }
  | Ctor      { _exprAnn      :: a
              , _exprCtorType :: PName 'TypeName
              , _exprCtorName :: PName 'ConstructorName
              , _exprCtorArgs :: [Ident] }
  | Abs       { _exprAnn     :: a
              , _exprAbsVar  :: Ident
              , _exprAbsBody :: Expr a }
  | App       { _exprAnn    :: a
              , _exprAppFun :: Expr a
              , _exprAppArg :: Expr a }
  | Case      { _exprAnn      :: a
              , _exprCaseVals :: [Expr a]
              , _exprCaseAlts :: [CaseAlternative a] }
  | Let       { _exprAnn      :: a
              , _exprLetBinds :: [Bind a]
              , _exprLetBody  :: Expr a }
  | RecAccess { _exprAnn      :: a
              , _exprRAField  :: Text
              , _exprRARecord :: Expr a }
  | RecUpdate { _exprAnn       :: a
              , _exprRURecord  :: Expr a
              , _exprRUChanges :: [(Text, Expr a)] }
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Annotated Expr where
  extractAnn = _exprAnn
  modifyAnn f e = e { _exprAnn = f $ _exprAnn e }

data Bind a
  = NonRec { _bindAnn  :: a
           , _bindName :: Ident
           , _bindVal  :: Expr a }
  | Rec    { _bindGroups :: [((a, Ident), Expr a)] }
  deriving (Show, Functor, Foldable, Traversable, Generic)

-- instance Annotated Bind where
--   extractAnn = _bindAnn
--   modifyAnn f b = b { _bindAnn = f $ _bindAnn b }

type Guard a = Expr a

data CaseAlternative a
  = CaseAlternative
    { _caseAltBinders :: [Binder a]
    , _caseAltResult  :: Either [(Guard a, Expr a)] (Expr a) }
  deriving (Show, Generic)

instance Functor CaseAlternative where
  fmap f (CaseAlternative cabs car)
    = let mapBindings = fmap (fmap f)
          mapResult   = bimap (fmap (fmap f *** fmap f)) (fmap f)
      in CaseAlternative (mapBindings cabs) (mapResult car)

instance Foldable CaseAlternative where
  foldMap f (CaseAlternative bs r) = foldMap (foldMap f) bs `mappend` res
    where
      res = either (foldMap procGuard) (foldMap f) r
      procGuard (g, e) = foldMap f g `mappend` foldMap f e

instance Traversable CaseAlternative where
  sequenceA (CaseAlternative bs r) = go
    where
      go = CaseAlternative
           <$> sequenceA (map sequenceA bs)
           <*> procEither (map procGuard .> sequenceA) sequenceA r
      procEither f g = either (f .> fmap Left) (g .> fmap Right)
      procGuard (g, e) = (,) <$> sequenceA g <*> sequenceA e

data Meta
  = IsConstructor ConstructorType [Ident]
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  deriving (Eq, Show, Generic)

data ConstructorType
  = ProductType
  | SumType
  deriving (Eq, Show, Generic)

data Module a
  = Module
    { _moduleComments :: [Comment]
    , _moduleName     :: ModuleName
    , _moduleImports  :: [(a, ModuleName)]
    , _moduleExports  :: [Ident]
    , _moduleForeign  :: [Ident]
    , _moduleDecls    :: [Bind a] }
  deriving (Show, Generic)

parseModule :: HCS => AnnParser a -> (ModuleName, JSON) -> JParse (Version, Module a)
parseModule annP (name, v) = (,)
                             <$> backtrace "versionP" versionP v
                             <*> backtrace "moduleP"  moduleP  v
  where
    versionP (Object o) = do String s <- o .: "builtWith"
                             either (T.pack .> pure .> failureT) pure
                               $ runReadP parseVersion $ T.unpack s
    versionP _          = failureT ["versionP: not an object"]
    moduleP (Object o) = Module
                         <$> (((o .: "comments") >>= mapM parseComment)
                              <|> pure [])
                         <*> pure name
                         <*> ((o .: "imports")  >>= mapM parseImport)
                         <*> ((o .: "exports")  >>= mapM parseIdent)
                         <*> ((o .: "foreign")  >>= mapM parseIdent)
                         <*> ((o .: "decls")    >>= mapM (parseBind annP))
    moduleP _          = failureT ["moduleP: not an object"]
    parseComment :: HCS => JSON -> JParse Comment
    parseComment (Array [String "Line",  String t]) = pure $ PS.LineComment  t
    parseComment (Array [String "Block", String t]) = pure $ PS.BlockComment t
    parseComment _                                  = empty
    parseImport = runInsideAP annP
                  $ \a -> parseModuleName .> fmap (\mn -> (a, mn))

parseModules :: HCS => AnnParser ann -> JSON -> JParse [(Version, Module ann)]
parseModules annP = parseJSON >=> LHM.toList .> mapM parsePair
  where
    parsePair = first moduleNameFromString .> parseModule annP

moduleParse :: HCS => AnnParser ann -> Text -> JSON
            -> Either String (Version, Module ann)
moduleParse annP mn = A.parseEither
                      (curry (parseModule annP) (moduleNameFromString mn))

-- newtype TopLevel = TopLevel [(Version, Module JSON)]
--                  deriving (Show)
--
-- instance FromJSON TopLevel where
--   parseJSON v = TopLevel <$> parseModules annP v

readGZ :: HCS => FilePath -> IO LBS.ByteString
readGZ = LBS.readFile .> fmap GZ.decompress

readJSON :: HCS => (FromJSON a) => LBS.ByteString -> IO a
readJSON = A.eitherDecode .> either (T.pack .> pure .> failureT) pure

simpleFile, bigFile, completeFile, fullFile :: HCS => IO JSON
simpleFile   = readGZ "./data/simple.json.gz"   >>= readJSON
bigFile      = readGZ "./data/big.json.gz"      >>= readJSON
completeFile = readGZ "./data/complete.json.gz" >>= readJSON
fullFile     = readGZ "./data/full.json.gz"     >>= readJSON

maybeIO :: HCS => Maybe a -> IO a
maybeIO = maybe (failureT ["maybeIO"]) pure

eitherIO' :: HCS => (e -> String) -> Either e a -> IO a
eitherIO' f = either (f .> T.pack .> pure .> failureT) pure

eitherIO :: HCS => (Show s) => Either s a -> IO a
eitherIO = eitherIO' show

printMap :: HCS => (Show s) => [(Text, s)] -> IO ()
printMap ps = do
  let longest = ps |> map (fst .> T.length) |> maximum
  let pad t = t <> T.replicate (longest - T.length t) " "
  let esc n = "\ESC[" <> T.pack (show n) <> "m"
  let color n t = esc n <> t <> esc 0
  forM_ ps $ \(name, size) -> [ color 35 $ pad name
                              , color 32 " -> "
                              , color 33 $ T.pack (show size)
                              ] |> mconcat |> T.putStrLn

computeSizes :: HCS => JSON -> [(Text, Int)]
computeSizes (Object obj) = obj
                            |> LHM.map jsonSize
                            |> LHM.toList
                            |> sortBy (comparing snd)
computeSizes _            = []

jsonSize :: HCS => JSON -> Int
jsonSize (Object o) = o |> LHM.toList |> fmap (snd .> jsonSize) |> sum
jsonSize (Array  v) = v |> V.map jsonSize |> V.sum
jsonSize _          = 1

deriving instance Generic (Literal a)
instance (ToJSON a) => ToJSON (CaseAlternative a)
instance (ToJSON a) => ToJSON (Literal a)
instance (ToJSON a) => ToJSON (Expr a)
instance (ToJSON a) => ToJSON (Binder a)
instance (ToJSON a) => ToJSON (Bind a)
instance (ToJSON a) => ToJSON (Module a)










(∋) :: HCS => DTerm -> DTerm -> TCM ()
DType ∋ DType                    = pure ()
DType ∋ (DΠ v vT bT)             = DType ∋ vT >> withVars [v := vT] (DType ∋ bT)
(DΠ v vT bT) ∋ (Dλ n b) | v == n = withVars [v := vT] (b ∋ bT)
ty ∋ (D_ e)                      = do inferred <- inferType e
                                      success <- ty === inferred
                                      unless success
                                        $ throwTypeSynthesisError ty (D_ e)
ty ∋ tm                          = throwTypeSynthesisError ty tm

inferType :: HCS => DElim -> TCM DTerm
inferType (tm ::: ty) = DType ∋ ty >> ty ∋ tm >> eval ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           eval $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = lookupVar n >>= eval

main :: HCS => IO ()
main = do
  Right complete <- completeFile <#> A.parseEither (parseModules defAnnP)
  A.encode complete |> GZ.compress |> Base64.encode |> LBS.putStrLn
  pure ()

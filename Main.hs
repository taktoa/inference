{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
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
import           Data.Bifunctor
import           Data.Foldable                     (asum)
import           Data.Semigroup

import           Data.Kind
import           GHC.Generics                      (Generic)
import           GHC.TypeLits

import           Data.List                         (maximumBy, sortBy)
import           Data.Ord                          (comparing)

import           Control.Lens                      hiding
                 ((.>), (<.), (<|), (|>))
import           Data.Aeson.Lens

import           Evaluable
import           Types
import           Utils

import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V

import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS

import qualified Codec.Compression.GZip            as GZ

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map

import qualified Data.HashMap.Lazy                 as LHM

import           Data.Aeson                        as A
-- import qualified Data.Aeson.BetterErrors           as A
import qualified Data.Aeson.Encode.Pretty          as A
import qualified Data.Aeson.Types                  as A

import           Data.Version
import           Safe

import qualified Language.PureScript.AST.Literals  as PS
import qualified Language.PureScript.AST.SourcePos as PS
import qualified Language.PureScript.Comments      as PS
import qualified Language.PureScript.Names         as PS
import qualified Language.PureScript.Types         as PS

import           Language.PureScript.AST.Literals  (Literal (..))
import           Language.PureScript.Names

backtrace :: Text -> (JSON -> JParse a) -> JSON -> JParse a
backtrace msg f v = f v <|> fail (T.unpack msg)

choice :: (Alternative f) => [f a] -> f a
choice = asum

instance Foldable Literal where
  foldMap f (ArrayLiteral  xs) = foldMap f xs
  foldMap f (ObjectLiteral xs) = foldMap (snd .> f) xs
  foldMap f owise              = mempty

instance Traversable Literal where
  sequenceA = helper
    where
      helper (NumericLiteral n) = pure $ NumericLiteral n
      helper (StringLiteral  s) = pure $ StringLiteral s
      helper (CharLiteral    c) = pure $ CharLiteral c
      helper (BooleanLiteral b) = pure $ BooleanLiteral b
      helper (ArrayLiteral  xs) = ArrayLiteral  <$> sequenceA xs
      helper (ObjectLiteral ps) = ObjectLiteral <$> sequenceA (map sequenceA ps)

type JParse a = A.Parser a
type JSON = Value

class (Functor f) => Annotated f where
  extractAnn :: f a -> a
  modifyAnn  :: (a -> a) -> f a -> f a

type PName  (a :: ProperNameType) = ProperName a
type QPName (a :: ProperNameType) = Qualified (ProperName a)
type QIdent     = Qualified Ident
type SourceSpan = PS.SourceSpan
type Comment    = PS.Comment

type Ann = (Maybe SourceSpan, [Comment], Maybe PS.Type, Maybe Meta)

nullAnn :: Ann
nullAnn = (Nothing, [], Nothing, Nothing)

removeComments :: Ann -> Ann
removeComments (mss, _, mt, mm) = (mss, [], mt, mm)

data Binder a
  = NullBinder    { _binderAnn      :: a }
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
  deriving (Show, Functor, Foldable, Traversable)

maybeToAlt :: (Alternative f) => Maybe a -> f a
maybeToAlt = maybe empty pure

parseTagList :: [(Text, JSON -> JParse a)] -> JSON -> JParse a
parseTagList mapping = backtrace "parseTagList"
                       $ \val -> do (k, v) <- helper val
                                    cb <- maybeToAlt (lookup k mapping)
                                    cb v
  where
    helper (String k)                               = pure (k, Null)
    helper (Array (uncons -> Just (String k, [v]))) = pure (k, v)
    helper (Array (uncons -> Just (String k, v)))   = pure (k, Array v)
    helper _                                        = empty

parseModuleName :: JSON -> JParse ModuleName
parseModuleName = backtrace "parseModuleName"
                  $ parseJSON .> fmap moduleNameFromString

parseIdent :: JSON -> JParse Ident
parseIdent = backtrace "parseIdent"
             $ parseJSON .> fmap Ident

parseBind :: (JSON -> a) -> JSON -> JParse (Bind a)
parseBind cb = backtrace "parseBind"
               $ withObject "parseBind: not an object" (LHM.toList .> helper)
  where
    helper [(n, v)] = NonRec (cb Null) (Ident n) <$> parseExpr cb v
    helper xs       = Rec (cb Null) <$> mapM mkRecBinding xs
    mkRecBinding (k, v) = (\e -> ((cb Null, Ident k), e)) <$> parseExpr cb v

parseProperName :: JSON -> JParse (ProperName a)
parseProperName = backtrace "parseProperName" (parseJSON .> fmap ProperName)

parseQualified :: forall a. (Text -> JParse a) -> JSON -> JParse (Qualified a)
parseQualified p = backtrace "parseQualified"
                   $ withText "parseQualified: not a string"
                   $ \s -> qualifiedP s <|> unqualifiedP s
  where
    qualifiedP, unqualifiedP :: Text -> JParse (Qualified a)
    qualifiedP s = case unsnoc (T.split (== '.') s)
                   of Just (xs, x) -> let mn = mkModName xs
                                      in Qualified (Just mn) <$> p x
                      Nothing      -> empty
    unqualifiedP s = Qualified Nothing <$> p s
    mkModName = moduleNameFromString . T.intercalate "."

parseLiteral :: (JSON -> JParse a) -> JSON -> JParse (Literal a)
parseLiteral cb = backtrace "parseLiteral"
                  $ parseTagList dispatch >=> (fmap cb .> sequenceA)
  where
    dispatch :: [(Text, JSON -> JParse (Literal JSON))]
    dispatch = [ ("IntLiteral",     go (NumericLiteral . Left))
               , ("NumberLiteral",  go (NumericLiteral . Right))
               , ("StringLiteral",  go StringLiteral)
               , ("CharLiteral",    go CharLiteral)
               , ("BooleanLiteral", go BooleanLiteral)
               , ("ArrayLiteral",   go ArrayLiteral)
               , ("ObjectLiteral",  go ObjectLiteral) ]
    go :: (FromJSON a) => (a -> Literal JSON) -> JSON -> JParse (Literal JSON)
    go f = parseJSON .> fmap f

parseBinder :: (JSON -> a) -> JSON -> JParse (Binder a)
parseBinder cb = backtrace "parseBinder"
                 (parseTagList [ ("NullBinder",        nullP)
                               , ("LiteralBinder",     litP)
                               , ("VarBinder",         varP)
                               , ("ConstructorBinder", ctorP)
                               , ("NamedBinder",       namedP) ]
                  .> fmap (fmap cb))
  where
    nullP, litP, varP, ctorP, namedP :: JSON -> JParse (Binder JSON)
    nullP    = pure . NullBinder
    litP     = parseLiteral (parseBinder id) .> fmap (LiteralBinder Null)
    varP     = withList "parseBinder: varP: not an array"
               $ \case [x] -> VarBinder Null <$> parseIdent x
                       _   -> empty
    ctorP    = withList "parseBinder: ctorP: not an array"
               $ \case [vt, vn, vbs] -> CtorBinder Null
                                        <$> parseQPName vt
                                        <*> parseQPName vn
                                        <*> parseBinderList id vbs
                       _             -> empty
    namedP   = withList "parseBinder: namedP: not an array"
               $ \case [vi, vb] -> NamedBinder Null
                                   <$> parseIdent vi
                                   <*> parseBinder id vb
                       _        -> empty

    withList err lam = withArray err (V.toList .> lam)

    parseBinderList :: (JSON -> a) -> JSON -> JParse [Binder a]
    parseBinderList f = parseJSON >=> mapM (parseBinder f)

    parseQPName :: JSON -> JParse (QPName a)
    parseQPName = parseQualified (ProperName .> pure)

parseCaseAlt :: (JSON -> a) -> JSON -> JParse (CaseAlternative a)
parseCaseAlt cb = backtrace "parseCaseAlt"
                  ((withArray "parseCaseAlt: not an array"
                    $ \case [vbs, vr] -> CaseAlternative
                                         <$> bindersP vbs
                                         <*> resultP vr
                            _         -> empty)
                   .> fmap (fmap cb))
  where
    bindersP :: JSON -> JParse [Binder JSON]
    bindersP = withArray "parseCaseAlt: bindersP: not an array"
               (V.mapM (parseBinder id) .> fmap V.toList)
    resultP :: JSON -> JParse (Either [(Guard JSON, Expr JSON)] (Expr JSON))
    resultP v = [ Left  <$> (parseJSON v >>= mapM guardP)
                , Right <$> parseExpr id v
                ] |> choice
    guardP :: (JSON, JSON) -> JParse (Guard JSON, Expr JSON)
    guardP (g, e) = (,) <$> parseExpr id g <*> parseExpr id e

parseExpr :: (JSON -> a) -> JSON -> JParse (Expr a)
parseExpr cb = backtrace "parseExpr"
               (parseTagList [ ("Literal",      litP)
                             , ("Constructor",  ctorP)
                             , ("Abs",          absP)
                             , ("App",          appP)
                             , ("Var",          varP)
                             , ("Case",         caseP)
                             , ("Let",          letP)
                             , ("Accessor",     raP)
                             , ("ObjectUpdate", ruP) ]
                .> fmap (fmap cb))
  where
    exprP, litP, varP, ctorP, absP, appP, caseP, letP, raP, ruP
      :: JSON -> JParse (Expr JSON)
    exprP = parseExpr id
    litP  = parseLiteral exprP .> fmap (Lit Null)
    varP  = parseQualified (Ident .> pure) .> fmap (Var Null)
    ctorP = wrapP $ \(t, n, as) -> Ctor Null
                                   <$> parseProperName t
                                   <*> parseProperName n
                                   <*> (parseJSON as >>= mapM parseIdent)
    absP  = wrapP $ \(n, b)     -> Abs Null <$> parseIdent n <*> exprP b
    appP  = wrapP $ \(f, x)     -> App Null <$> exprP f <*> exprP x
    caseP = wrapP $ \(vs, as)   -> Case Null
                                   <$> arrayP vs exprP
                                   <*> arrayP as (parseCaseAlt id)
    letP  = wrapP $ \(bs, e)    -> Let Null
                                   <$> arrayP bs (parseBind id)
                                   <*> exprP e
    raP   = wrapP $ \(f, r)     -> RecAccess Null
                                   <$> parseJSON f
                                   <*> exprP r
    ruP   = wrapP $ \(r, fs)    -> RecUpdate Null
                                   <$> exprP r
                                   <*> arrayP fs (second exprP .> sequenceA)
    arrayP :: (FromJSON a) => JSON -> (a -> JParse b) -> JParse [b]
    arrayP v f = wrapP (mapM f) v
    wrapP :: (FromJSON a) => (a -> JParse b) -> JSON -> JParse b
    wrapP f v = parseJSON v >>= f

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
  deriving (Show, Functor, Foldable, Traversable)

instance Annotated Expr where
  extractAnn = _exprAnn
  modifyAnn f e = e { _exprAnn = f $ _exprAnn e }

data Bind a
  = NonRec { _bindAnn  :: a
           , _bindName :: Ident
           , _bindVal  :: Expr a }
  | Rec    { _bindAnn    :: a
           , _bindGroups :: [((a, Ident), Expr a)] }
  deriving (Show, Functor, Foldable, Traversable)

instance Annotated Bind where
  extractAnn = _bindAnn
  modifyAnn f b = b { _bindAnn = f $ _bindAnn b }

type Guard a = Expr a

data CaseAlternative a
  = CaseAlternative
    { _caseAltBinders :: [Binder a]
    , _caseAltResult  :: Either [(Guard a, Expr a)] (Expr a) }
  deriving (Show)

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
  deriving (Eq, Show)

data ConstructorType
  = ProductType
  | SumType
  deriving (Eq, Show)

data Module a
  = Module
    { _moduleComments :: [Comment]
    , _moduleName     :: ModuleName
    , _moduleImports  :: [(a, ModuleName)]
    , _moduleExports  :: [Ident]
    , _moduleForeign  :: [Ident]
    , _moduleDecls    :: [Bind a] }
  deriving (Show)

parseModule :: (ModuleName, JSON) -> JParse (Version, Module JSON)
parseModule (name, v) = (,)
                        <$> backtrace "versionP" versionP v
                        <*> backtrace "moduleP"  moduleP  v
  where
    versionP (String s) = maybe empty pure $ readMay (T.unpack s)
    versionP _          = empty
    moduleP (Object o) = Module
                         <$> pure []
                         <*> pure name
                         <*> ((o .: "imports") >>= mapM parseImport)
                         <*> ((o .: "exports") >>= mapM parseIdent)
                         <*> ((o .: "foreign") >>= mapM parseIdent)
                         <*> ((o .: "decls")   >>= mapM (parseBind id))
    moduleP _          = empty
    parseImport = parseModuleName .> fmap (\mn -> (Null, mn))

parseModules :: JSON -> JParse [(Version, Module JSON)]
parseModules = parseJSON >=> mapM (first moduleNameFromString .> parseModule)

newtype TopLevel = TopLevel [(Version, Module JSON)]
                 deriving (Show)

instance FromJSON TopLevel where
  parseJSON v = TopLevel <$> parseModules v

readGZ :: FilePath -> IO LBS.ByteString
readGZ = LBS.readFile .> fmap GZ.decompress

readJSON :: (FromJSON a) => LBS.ByteString -> IO a
readJSON = A.eitherDecode .> either fail pure

simpleFile, bigFile, completeFile :: IO JSON
simpleFile   = readGZ "./data/simple.json.gz"   >>= readJSON
bigFile      = readGZ "./data/big.json.gz"      >>= readJSON
completeFile = readGZ "./data/complete.json.gz" >>= readJSON

maybeIO :: Maybe a -> IO a
maybeIO = maybe (fail "maybeIO") pure

eitherIO' :: (e -> String) -> Either e a -> IO a
eitherIO' f = either (f .> fail) pure

eitherIO :: (Show s) => Either s a -> IO a
eitherIO = eitherIO' show

printMap :: (Show s) => [(Text, s)] -> IO ()
printMap ps = do
  let longest = ps |> map (fst .> T.length) |> maximum
  let pad t = t <> T.replicate (longest - T.length t) " "
  let esc n = "\ESC[" <> T.pack (show n) <> "m"
  let color n t = esc n <> t <> esc 0
  forM_ ps $ \(name, size) -> [ color 35 $ pad name
                              , color 32 " -> "
                              , color 33 $ T.pack (show size)
                              ] |> mconcat |> T.putStrLn

computeSizes :: JSON -> [(Text, Int)]
computeSizes (Object obj) = obj
                            |> LHM.map jsonSize
                            |> LHM.toList
                            |> sortBy (comparing snd)
computeSizes _            = []

jsonSize :: JSON -> Int
jsonSize (Object o) = o |> LHM.toList |> fmap (snd .> jsonSize) |> sum
jsonSize (Array  v) = v |> V.map jsonSize |> V.sum
jsonSize _          = 1

(∋) :: DTerm -> DTerm -> TCM ()
DType ∋ DType                    = pure ()
DType ∋ (DΠ v vT bT)             = DType ∋ vT >> withVars [v := vT] (DType ∋ bT)
(DΠ v vT bT) ∋ (Dλ n b) | v == n = withVars [v := vT] (b ∋ bT)
ty ∋ (D_ e)                      = do inferred <- inferType e
                                      success <- ty === inferred
                                      unless success
                                        $ throwTypeSynthesisError ty (D_ e)
ty ∋ tm                          = throwTypeSynthesisError ty tm

inferType :: DElim -> TCM DTerm
inferType (tm ::: ty) = DType ∋ ty >> ty ∋ tm >> eval ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           eval $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = lookupVar n >>= eval

debug :: DElim
debug = (Dλ "x" (D_ "x")) ::: (DΠ "x" DType (D_ "x"))

main :: IO ()
main = do
  pure ()

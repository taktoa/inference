{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module PureScript.Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.TH                     as A
import qualified Data.Text                         as T
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Comments
import           Language.PureScript.Names
import           Language.PureScript.PSString      (PSString)

data Module a = Module
  { moduleComments :: [Comment]
  , moduleName     :: ModuleName
  , moduleImports  :: [(a, ModuleName)]
  , moduleExports  :: [Ident]
  , moduleForeign  :: [ForeignDecl]
  , moduleDecls    :: [Bind a]
  } deriving (Show)

type ForeignDecl = (Ident, Type)

$(A.deriveJSON A.defaultOptions ''Module)

data Expr a
  = Literal a (Literal (Expr a))
  | Constructor a (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
  | Accessor a PSString (Expr a)
  | ObjectUpdate a (Expr a) [(PSString, Expr a)]
  | Abs a Ident (Expr a)
  | App a (Expr a) (Expr a)
  | Var a (Qualified Ident)
  | Case a [Expr a] [CaseAlternative a]
  | Let a [Bind a] (Expr a)
  deriving (Show, Functor)

$(A.deriveJSON A.defaultOptions ''Expr)

data Bind a
  = NonRec a Ident (Expr a)
  | Rec [((a, Ident), Expr a)]
  deriving (Show, Functor)

$(A.deriveJSON A.defaultOptions ''Bind)

type Guard a = Expr a

data CaseAlternative a
  = CaseAlternative
    { caseAlternativeBinders :: [Binder a]
    , caseAlternativeResult  :: Either [(Guard a, Expr a)] (Expr a)
    }
  deriving (Show)

instance Functor CaseAlternative where
  fmap f (CaseAlternative bs r)
    = CaseAlternative
      (fmap (fmap f) bs)
      (bimap (fmap (fmap f *** fmap f)) (fmap f) r)

$(A.deriveJSON A.defaultOptions ''CaseAlternative)

data Binder ann
  = NullBinder ann
  | LiteralBinder ann
    (Literal (Binder ann))
  | VarBinder ann
    Ident
  | ConstructorBinder ann
    (Qualified (ProperName 'TypeName))
    (Qualified (ProperName 'ConstructorName))
    [Binder ann]
  | NamedBinder ann
    Ident
    (Binder ann)
  deriving (Show, Functor)

$(A.deriveJSON A.defaultOptions ''Binder)

data Literal a
  = NumericLiteral (Either Integer Double)
  | StringLiteral PSString
  | CharLiteral Char
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  | ObjectLiteral [(PSString, a)]
  deriving (Eq, Ord, Show, Functor)

$(A.deriveJSON A.defaultOptions ''Literal)

data Meta
  = IsConstructor ConstructorType [Ident]
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  deriving (Show, Eq)

instance ToJSON Meta where
  toJSON (IsConstructor ct as)  = toJSON (T.pack "IsConstructor", ct, as)
  toJSON IsNewtype              = toJSON $ T.pack "IsNewtype"
  toJSON IsTypeClassConstructor = toJSON $ T.pack "IsTCConstructor"
  toJSON IsForeign              = toJSON $ T.pack "IsForeign"

instance FromJSON Meta where
  parseJSON (String "IsNewtype")       = pure IsNewtype
  parseJSON (String "IsTCConstructor") = pure IsTypeClassConstructor
  parseJSON (String "IsForeign")       = pure IsForeign
  parseJSON owise                      = do
    tuple <- parseJSON owise
    let ic = T.pack "IsConstructor"
    case tuple of (tag, ct, as) | tag == ic -> pure $ IsConstructor ct as
                  _             -> empty

data ConstructorType
  = ProductType
  | SumType
  deriving (Show, Eq)

instance ToJSON ConstructorType where
  toJSON ProductType = "Product"
  toJSON SumType     = "Sum"

instance FromJSON ConstructorType where
  parseJSON (String "Product") = pure ProductType
  parseJSON (String "Sum")     = pure SumType
  parseJSON _                  = empty

type Ann = (Maybe SourceSpan, [Comment], Maybe Type, Maybe Meta)

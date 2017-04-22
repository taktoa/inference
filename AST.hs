module AST where

data Module a = Module
  { moduleComments :: [Comment]
  , moduleName     :: ModuleName
  , moduleImports  :: [(a, ModuleName)]
  , moduleExports  :: [Ident]
  , moduleForeign  :: [ForeignDecl]
  , moduleDecls    :: [Bind a]
  } deriving (Show)

type ForeignDecl = (Ident, Type)

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

data Bind a
  = NonRec a Ident (Expr a)
  | Rec [((a, Ident), Expr a)]
  deriving (Show, Functor)

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

data Literal a
  = NumericLiteral (Either Integer Double)
  | StringLiteral PSString
  | CharLiteral Char
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  | ObjectLiteral [(PSString, a)]
  deriving (Eq, Ord, Show, Functor)

data Meta
  = IsConstructor ConstructorType [Ident]
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  deriving (Show, Eq)

data ConstructorType
  = ProductType
  | SumType
  deriving (Show, Eq)

type Ann = (Maybe SourceSpan, [Comment], Maybe Type, Maybe Meta)

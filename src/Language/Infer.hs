{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE PolyKinds            #-}
module Language.Infer where

import           Prelude                      hiding (pi)

import           Data.String                  (IsString (..))

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import qualified Data.Text.Lazy               as LT (toStrict)
import qualified Data.Text.Lazy.Builder       as LT

import           TextShow                     (TextShow (..))
import qualified TextShow                     as T

import           Data.Functor.Const           (Const (..))
import           Data.Functor.Product         (Product (..))
import           Data.HFunctor.Foldable
                 (HFunctor (..), type Base, Recursive (..), type (~>))

import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Language.Infer.Utils

newtype Name
  = Name { getName :: Text }
  deriving (Eq, Show, Read, Generic)

data Var a
  = Bound {-# UNPACK #-} !Int
  | Free                 !a
  deriving (Eq, Show, Read, Generic, Foldable, Functor, Traversable)

data Named a
  = Named
    { namedName :: {-# UNPACK #-} !Name
    , namedVar  ::                !(Var a)
    }
  deriving (Eq, Show, Read, Generic, Foldable, Functor, Traversable)

instance TextShow (Named a) where
  showb = namedName .> getName .> LT.fromText

instance Pretty (Named a) where
  pretty = namedName .> getName .> T.unpack .> PP.text

instance IsString Name where
  fromString = T.pack .> Name

named :: Text -> Named Text
named t = Named (Name t) (Free t)

data Kind
  = Term
  | Elim

data AST v u c (k :: Kind) where
  Universe :: u -> AST v u c Term
  Constant :: c -> AST v u c Term
  Embed    :: AST v u c Elim -> AST v u c Term
  Pi       :: Named v -> AST v u c Term -> AST v u c Term -> AST v u c Term
  Lam      :: Named v -> AST v u c Term -> AST v u c Term
  Ref      :: Named v -> AST v u c Elim
  (:::)    :: AST v u c Term -> AST v u c Term -> AST v u c Elim
  (:@:)    :: AST v u c Elim -> AST v u c Term -> AST v u c Elim

deriving instance (Show v, Show u, Show c) => Show (AST v u c k)

data ASTF v u c f (k :: Kind) where
  UniverseF :: u                           -> ASTF v u c f Term
  ConstantF :: c                           -> ASTF v u c f Term
  EmbedF    :: f Elim                      -> ASTF v u c f Term
  PiF       :: Named v -> f Term -> f Term -> ASTF v u c f Term
  LamF      :: Named v -> f Term           -> ASTF v u c f Term
  RefF      :: Named v                     -> ASTF v u c f Elim
  (:.:.:)   :: f Term  -> f Term           -> ASTF v u c f Elim
  (:.@.:)   :: f Elim  -> f Term           -> ASTF v u c f Elim

deriving instance (Show v, Show u, Show c, Show (f Term), Show (f Elim)) => Show (ASTF v u c f k)

instance HFunctor (ASTF v u c) where
  hfmap eta = (\case UniverseF u  -> UniverseF u
                     ConstantF c  -> ConstantF c
                     EmbedF e     -> EmbedF (eta e)
                     PiF v vT bT  -> PiF v (eta vT) (eta bT)
                     LamF v b     -> LamF v (eta b)
                     RefF v       -> RefF v
                     tm :.:.: ty  -> eta tm :.:.: eta ty
                     el :.@.: tm  -> eta el :.@.: eta tm)

type instance Base (AST v u c) = ASTF v u c

instance Recursive (AST v u c) where
  project = (\case Universe u -> UniverseF u
                   Constant c -> ConstantF c
                   Embed e    -> EmbedF e
                   Pi v vT bT -> PiF v vT bT
                   Lam v b    -> LamF v b
                   Ref v      -> RefF v
                   tm ::: ty  -> tm :.:.: ty
                   el :@: tm  -> el :.@.: tm)

pi :: Text -> AST Text u c Term -> AST Text u c Term -> AST Text u c Term
pi v = Pi (named v)

lam :: T.Text -> AST T.Text u c Term -> AST T.Text u c Term
lam v = Lam (named v)

ref :: Text -> AST Text u c Elim
ref v = Ref (named v)

refE :: Text -> AST Text u c Term
refE = Embed . ref

hsnd :: Product f g ~> g
hsnd (Pair _ g) = g

parensWhenBinder :: Product (AST v u c) (Const LT.Builder) Term -> LT.Builder
parensWhenBinder (Pair term (Const b)) = case term of
  Pi _ _ _ -> withParens
  Lam _ _  -> withParens
  _ -> b
  where withParens = "(" <> b <> ")"

-- | Print the AST in plain ASCII.
printAscii :: forall v u c k. (TextShow u, TextShow c) => AST v u c k -> Text
printAscii = para alg .> getConst .> LT.toLazyText .> LT.toStrict
  where
    alg :: ASTF v u c (AST v u c `Product` Const LT.Builder) ~> Const LT.Builder
    alg = (\case UniverseF u -> [ T.showb u ]
                 ConstantF c -> [ T.showb c ]
                 EmbedF e    -> [ "{", getConst (hsnd e), "}" ]
                 PiF v vT bT -> [ "(", T.showb v, " : ", parensWhenBinder vT, ")"
                                , " -> ", getConst (hsnd bT) ]
                 LamF v b    -> [ "\\", T.showb v, " -> ", getConst (hsnd b) ]
                 RefF v      -> [ T.showb v ]
                 tm :.:.: ty -> [ getConst (hsnd tm), " : ", parensWhenBinder ty ]
                 el :.@.: tm -> [ getConst (hsnd el), " ", parensWhenBinder tm ])
          .> mconcat
          .> Const

-- | Print the AST with fancy Unicode.
printUnicode :: forall v u c k. (TextShow u, TextShow c) => AST v u c k -> Text
printUnicode = para alg .> getConst .> LT.toLazyText .> LT.toStrict
  where
    alg :: ASTF v u c (AST v u c `Product` Const LT.Builder) ~> Const LT.Builder
    alg = (\case UniverseF u -> [ T.showb u ]
                 ConstantF c -> [ T.showb c ]
                 EmbedF el   -> [ "⸨", getConst (hsnd el), "⸩" ]
                 PiF v vT bT -> [ "⟨", T.showb v, " : ", parensWhenBinder vT, "⟩"
                                , " → ", getConst (hsnd bT) ]
                 LamF v b    -> [ "λ", T.showb v, " ↦ ", getConst (hsnd b) ]
                 RefF v      -> [ T.showb v ]
                 tm :.:.: ty -> [ getConst (hsnd tm), " ∷ ", parensWhenBinder ty ]
                 el :.@.: tm -> [ getConst (hsnd el), " ", parensWhenBinder tm ])
          .> mconcat
          .> Const

instance (Pretty u, Pretty c) => Pretty (AST v u c k) where
  pretty = para alg .> getConst
    where
      parensWhenBinderDoc :: Product (AST v u c) (Const Doc) Term -> Doc
      parensWhenBinderDoc (Pair term (Const b)) = case term of
        Pi _ _ _ -> PP.parens b
        Lam _ _  -> PP.parens b
        _ -> b

      alg :: ASTF v u c (AST v u c `Product` Const Doc) ~> Const Doc
      alg = (\case UniverseF u -> [ pretty u ]
                   ConstantF c -> [ pretty c ]
                   EmbedF el   -> [ "⸨", getConst (hsnd el), "⸩" ]
                   PiF v vT bT -> [ "⟨", pretty v, " : ", parensWhenBinderDoc vT, "⟩"
                                 , " → ", getConst (hsnd bT) ]
                   LamF v b    -> [ "λ", pretty v, " ↦ ", getConst (hsnd b) ]
                   RefF v      -> [ pretty v ]
                   tm :.:.: ty -> [ getConst (hsnd tm), " ∷ ", parensWhenBinderDoc ty ]
                   el :.@.: tm -> [ getConst (hsnd el), " ", parensWhenBinderDoc tm ])
            .> mconcat
            .> Const

nameless :: AST v u c k -> AST v u c k
nameless = undefined -- FIXME: This function should de burjin free variables 

shift :: Int -> AST v u c k -> AST v u c k
shift = undefined -- FIXME: write definition

subst :: Named v -> AST v u c Elim -> AST v u c Term -> AST v u c Term
subst = undefined -- FIXME: write definition

data Star
  = Star

instance T.TextShow Star where
  showb Star = "⋆"

instance Pretty Star where
  pretty Star = "⋆"

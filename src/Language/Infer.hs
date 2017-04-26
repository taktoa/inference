{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Language.Infer where

import Prelude hiding (pi)

import Data.Monoid ((<>))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified TextShow as T
import qualified Data.Text.Lazy as LT (toStrict)
import qualified Data.Text.Lazy.Builder as LT
import GHC.Generics

import Data.Functor.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.HFunctor.Foldable (HFix(..),type (~>),HFunctor(..),Recursive(..))

import Control.Monad.Reader (ReaderT(..))
import qualified Data.Map.Strict as M

newtype Name = Name
  { getName :: T.Text }
  deriving (Eq, Show, Read, Generic)

data Var a
  = Bound {-# UNPACK #-} !Int
  | Free !a
  deriving (Eq, Show, Read, Generic,
            Foldable, Functor, Traversable)

data Named a = Named
  { namedName  :: {-# UNPACK #-} !Name
  , namedVar :: !(Var a)
  } deriving (Eq, Show, Read, Generic,
              Foldable, Functor, Traversable)

instance T.TextShow (Named a) where
  showb = LT.fromText . getName . namedName

instance IsString Name where
  fromString = Name . T.pack

named :: T.Text -> Named T.Text
named t = Named (Name t) (Free t)

data Kind = Term | Elim

data ASTF v u c f (k :: Kind) where
  Universe :: u -> ASTF v u c f Term
  Constant :: c -> ASTF v u c f Term
  Embed :: f Elim -> ASTF v u c f Term
  Pi :: Named v -> f Term -> f Term -> ASTF v u c f Term
  Lam :: Named v -> f Term -> ASTF v u c f Term
  Ref :: Named v -> ASTF v u c f Elim
  (:::) :: f Term -> f Term -> ASTF v u c f Elim
  (:@:) :: f Elim -> f Term -> ASTF v u c f Elim

deriving instance (Show v, Show u, Show c, Show (f Term), Show (f Elim)) => Show (ASTF v u c f k)

instance HFunctor (ASTF v u c) where
  hfmap eta = \case
    Universe u -> Universe u
    Constant c -> Constant c
    Embed e -> Embed (eta e)
    Pi x s t -> Pi x (eta s) (eta t)
    Lam x t -> Lam x (eta t)
    Ref v -> Ref v
    t ::: typ -> eta t ::: eta typ
    f :@: s -> eta f :@: eta s

type AST v u c (k :: Kind) = HFix (ASTF v u c) k

universe :: u -> AST v u c Term
universe = HFix . Universe

constant :: c -> AST v u c Term
constant = HFix . Constant

embed :: AST v u c Elim -> AST v u c Term
embed = HFix . Embed

pi :: Named v -> AST v u c Term -> AST v u c Term -> AST v u c Term
pi x s = HFix . Pi x s

pi' :: T.Text -> AST T.Text u c Term -> AST T.Text u c Term -> AST T.Text u c Term
pi' x = pi (named x)

lam :: Named v -> AST v u c Term -> AST v u c Term
lam x = HFix . Lam x

ref :: Named v -> AST v u c Elim
ref = HFix . Ref

ref' :: T.Text -> AST T.Text u c Elim
ref' = ref . named

(.:.) :: AST v u c Term -> AST v u c Term -> AST v u c Elim
t .:. typ = HFix (t ::: typ)

(.@.) :: AST v u c Elim -> AST v u c Term -> AST v u c Elim
f .@. s = HFix (f :@: s)

pprintAST :: forall v u c k. (T.TextShow u, T.TextShow c) => AST v u c k -> T.Text
pprintAST = LT.toStrict . LT.toLazyText . getConst . cata alg
  where alg :: ASTF v u c (Const LT.Builder) ~> Const LT.Builder
        alg = Const . \case
          Universe u -> T.showb u
          Constant c -> T.showb c
          Embed e -> "{" <> getConst e <> "}"
          Pi x s t -> "(" <> T.showb x <> " : " <> getConst s <> ") -> " <> getConst t
          Lam x t -> "λ" <> T.showb x <> ". " <> getConst t
          Ref v -> T.showb v
          t ::: typ -> getConst t <> " : " <> getConst typ
          f :@: s -> getConst f <> " " <> getConst s

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
import           Data.HFunctor.Foldable
                 (HFix (..), HFunctor (..), Recursive (..), type (~>))

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

data ASTF v u c f (k :: Kind) where
  Universe :: u                           -> ASTF v u c f Term
  Constant :: c                           -> ASTF v u c f Term
  Embed    :: f Elim                      -> ASTF v u c f Term
  Pi       :: Named v -> f Term -> f Term -> ASTF v u c f Term
  Lam      :: Named v -> f Term           -> ASTF v u c f Term
  Ref      :: Named v                     -> ASTF v u c f Elim
  (:::)    :: f Term  -> f Term           -> ASTF v u c f Elim
  (:@:)    :: f Elim  -> f Term           -> ASTF v u c f Elim

deriving instance (Show v, Show u, Show c, Show (f Term), Show (f Elim)) => Show (ASTF v u c f k)

instance HFunctor (ASTF v u c) where
  hfmap eta = (\case Universe u -> Universe u
                     Constant c -> Constant c
                     Embed e    -> Embed (eta e)
                     Pi v vT bT -> Pi v (eta vT) (eta bT)
                     Lam v b    -> Lam v (eta b)
                     Ref v      -> Ref v
                     tm ::: ty  -> eta tm ::: eta ty
                     el :@: tm  -> eta el :@: eta tm)

type AST v u c (k :: Kind) = HFix (ASTF v u c) k

universe :: u -> AST v u c Term
universe u = HFix (Universe u)

constant :: c -> AST v u c Term
constant c = HFix (Constant c)

embed :: AST v u c Elim -> AST v u c Term
embed el = HFix (Embed el)

pi :: Named v -> AST v u c Term -> AST v u c Term -> AST v u c Term
pi v vT bT = HFix (Pi v vT bT)

pi' :: Text -> AST Text u c Term -> AST Text u c Term -> AST Text u c Term
pi' v = pi (named v)

lam :: Named v -> AST v u c Term -> AST v u c Term
lam v b = HFix (Lam v b)

lam' :: T.Text -> AST T.Text u c Term -> AST T.Text u c Term
lam' v = lam (named v)

ref :: Named v -> AST v u c Elim
ref v = HFix (Ref v)

ref' :: Text -> AST Text u c Elim
ref' v = ref (named v)

refE :: Text -> AST Text u c Term
refE = embed . ref'

(.:.) :: AST v u c Term -> AST v u c Term -> AST v u c Elim
tm .:. ty = HFix (tm ::: ty)

(.@.) :: AST v u c Elim -> AST v u c Term -> AST v u c Elim
el .@. tm = HFix (el :@: tm)

-- | Print the AST in plain ASCII.
printAscii :: forall v u c k. (TextShow u, TextShow c) => AST v u c k -> Text
printAscii = cata alg .> getConst .> LT.toLazyText .> LT.toStrict
  where
    alg :: ASTF v u c (Const LT.Builder) ~> Const LT.Builder
    alg = (\case Universe u -> [ T.showb u ]
                 Constant c -> [ T.showb c ]
                 Embed e    -> [ "{", getConst e, "}" ]
                 Pi v vT bT -> [ "(", "(", T.showb v, " : ", getConst vT, ")"
                               , " -> ", getConst bT, ")" ]
                 Lam v b    -> [ "(\\", T.showb v, " -> ", getConst b, ")" ]
                 Ref v      -> [ T.showb v ]
                 tm ::: ty  -> [ getConst tm, " : ", getConst ty ]
                 el :@: tm  -> [ getConst el, " ", getConst tm ])
          .> mconcat
          .> Const

-- | Print the AST with fancy Unicode.
printUnicode :: forall v u c k. (TextShow u, TextShow c) => AST v u c k -> Text
printUnicode = cata alg .> getConst .> LT.toLazyText .> LT.toStrict
  where
    alg :: ASTF v u c (Const LT.Builder) ~> Const LT.Builder
    alg = (\case Universe u -> [ T.showb u ]
                 Constant c -> [ T.showb c ]
                 Embed el   -> [ "⸨", getConst el, "⸩" ]
                 Pi v vT bT -> [ "(", "⟨", T.showb v, " : ", getConst vT, "⟩"
                               , " → ", getConst bT, ")" ]
                 Lam v b    -> [ "(", T.showb v, " ↦ ", getConst b, ")" ]
                 Ref v      -> [ T.showb v ]
                 tm ::: ty  -> [ getConst tm, " ∷ ", getConst ty ]
                 el :@: tm  -> [ getConst el, " ", getConst tm ])
          .> mconcat
          .> Const

instance (Pretty u, Pretty c) => Pretty (AST v u c k) where
  pretty = cata alg .> getConst
    where
      alg :: ASTF v u c (Const Doc) ~> Const Doc
      alg = (\case Universe u -> [ pretty u ]
                   Constant c -> [ pretty c ]
                   Embed el   -> [ "⸨", getConst el, "⸩" ]
                   Pi v vT bT -> [ "⟨", pretty v, " : ", getConst vT, "⟩"
                                 , " → ", getConst bT
                                 ] |> mconcat |> PP.parens |> pure @[]
                   Lam v b    -> [ "(", pretty v, " ↦ ", getConst b, ")" ]
                   Ref v      -> [ pretty v ]
                   tm ::: ty  -> [ getConst tm, " ∷ ", getConst ty ]
                   el :@: tm  -> [ getConst el, " ", getConst tm ])
            .> mconcat
            .> Const

shift :: Int -> AST v u c k -> AST v u c k
shift = undefined -- FIXME: write definition

subst :: a -- FIXME: write type
subst = undefined -- FIXME: write definition

data Star
  = Star

instance T.TextShow Star where
  showb Star = "⋆"

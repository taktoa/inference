{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Data.Monoid

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Hashable              (Hashable)
import           Data.String                (IsString (..))
import           GHC.Generics               (Generic)

import           Helpers
import           Supply

newtype Name
  = MkName Text
  deriving (Eq, Show, Read, Generic)

data Named a
  = Named
    { _namedName  :: {-# UNPACK #-} !Name
    , _namedValue :: {-# UNPACK #-} !a
    }
  deriving (Eq, Show, Read, Generic)

instance IsString Name where
  fromString = T.pack .> MkName

data DTerm' n
  = DStar
  | DConst !Text
  | D_ (DElim' n)
  | DΠ !n (DTerm' n) (DTerm' n)
  | Dλ !n (DTerm' n)
  deriving (Eq, Show, Read, Generic)

instance IsString DTerm where
  fromString = T.pack .> DConst

data DElim' n
  = (DTerm' n) ::: (DTerm' n)
  | (DElim' n) :@: (DTerm' n)
  | DRef n
  deriving (Eq, Show, Read, Generic)

instance IsString DElim where
  fromString = fromString .> DRef

type DTerm = DTerm' Name

type DElim = DElim' Name

data DConstraint
  = DTerm :≡: DTerm
  deriving (Eq, Show, Read, Generic)

data Context
  = MkContext
    { _environment :: HashMap Name DTerm
    , _constraints :: HashSet DConstraint
    }
  deriving (Eq, Show, Read, Generic)

instance Hashable Name
instance Hashable DTerm
instance Hashable DElim
instance Hashable DConstraint

instance Monoid Context where
  mempty = MkContext mempty mempty
  mappend (MkContext m1 s1) (MkContext m2 s2)
    = MkContext (m1 <> m2) (s1 <> s2)

newtype TCM a
  = TCM (StateT Context (Either String) a)
  deriving ( Functor, Applicative, Alternative
           , Monad, MonadPlus, MonadState Context )

data Replace = Name :~> DElim

-- instance Eq DTerm where
--   DStar           == DStar           = True
--   (DConst x)      == (DConst y)      = (x == y)
--   (D_ elX)        == (D_ elY)        = (elX == elY)
--   (DΠ vX vTX bTX) == (DΠ vY vTY bTY) = let sub t = subst t [vY :~> vX]
--                                        in (vTX == sub vTY) && (bTX == sub bTY)
--   (Dλ vX bX)      == (Dλ vY bY)      = _
--
-- instance Eq DElim where
--   (tmX ::: tyX) == (tmY ::: tyY) = (tmX == tmY) && (tyX == tyY)
--   (elX :@: tmX) == (elY :@: tmY) = (elX == elY) && (tmX == tmY)
--   (DRef nX)     == (DRef nY)     = (nX == nY)

prettyN :: Name -> Text
prettyN (MkName n) = n

prettyEl :: DElim -> Text
prettyEl (DRef n)    = prettyN n
prettyEl (tm ::: ty) = "(" <> pretty tm <> " ∷ " <> pretty ty <> ")"
prettyEl (el :@: tm) = prettyEl el <> " " <> pretty tm

pretty :: DTerm -> Text
pretty DStar        = "⋆"
pretty (DConst c)   = "c⟨" <> c <> "⟩"
pretty (D_ el)      = "⟦" <> prettyEl el <> "⟧"
pretty (DΠ v vT bT) = "(Π⟨" <> prettyN v <> " : " <> pretty vT <> "⟩"
                      <> " " <> pretty bT <> ")"
pretty (Dλ v b)     = "(λ" <> prettyN v <> " → " <> pretty b <> ")"

subst :: DTerm -> [Replace] -> DTerm
subst term []                      = term
subst term ((name :~> value):rest) = subst (subTm name value term) rest
  where
    subTm :: Name -> DElim -> DTerm -> DTerm
    subTm _ _ DStar                = DStar
    subTm _ _ (DConst s)           = DConst s
    subTm n r (D_ el)              = D_ $ subEl n r el
    subTm n r (DΠ v vT bT) | n ≢ v = let vT' = subTm n r vT
                                         bT' = subTm n r bT
                                     in DΠ v vT' bT'
    subTm n r (Dλ v b)     | n ≢ v = Dλ v $ subTm n r b
    subTm _ _ other                = other

    subEl :: Name -> DElim -> DElim -> DElim
    subEl n r (tm ::: ty)           = subTm n r tm ::: subTm n r ty
    subEl n r (el :@: ty)           = subEl n r el :@: subTm n r ty
    subEl n r (DRef var)  | n ≡ var = r
    subEl _ _ other                 = other

tell :: (Monad m, Monoid s) => s -> StateT s m ()
tell s = modify (<> s)

with :: [(Name, DTerm)] -> TCM a -> TCM a
with bs m = TCM (tell (Ctx (Map.fromList bs) mempty)) >> m

liftMaybe :: [Text] -> Maybe a -> TCM a
liftMaybe _ (Just x) = pure x
liftMaybe e Nothing  = failure e

retrieveVar :: Name -> TCM DTerm
retrieveVar n = let err = ["Could not find variable: ", tshow n]
                in get >>= _environment .> Map.lookup n .> liftMaybe err

(∋) :: DTerm -> DTerm -> TCM ()
(∋) = \ty tm -> trace (pretty ty <> " ∋ " <> pretty tm) (check tm ty)
  where
    check :: DTerm -> DTerm -> TCM ()
    check DStar        DStar        = pure ()
    check DStar        (DΠ v vT bT) = do DStar ∋ vT
                                         with [(v, vT)] (DStar ∋ bT)
    check (DΠ v vT bT) (Dλ n b)     = with [(v, vT)] $ b ∋ bT
                                      -- FIXME: replace to common name
    check ty           (D_ e)       = do tm  <- inferType e
                                         tm' <- evaluateTerm tm
                                         ty' <- evaluateTerm ty
                                         guard $ tm' ≡ ty'
    check ty           tm           = [pretty ty, " ∌ ", pretty tm] |> failure

inferType :: DElim -> TCM DTerm
inferType (tm ::: ty) = do DStar ∋ ty
                           ty ∋ tm
                           evaluateTerm ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           evaluateTerm $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = retrieveVar n >>= evaluateTerm

evaluateTerm :: DTerm -> TCM DTerm
evaluateTerm DStar      = pure DStar
evaluateTerm (DConst i) = pure $ DConst i
evaluateTerm (D_ el)    = evaluateElim el
evaluateTerm term       = fail ("evaluateTerm: " <> show term)

evaluateElim :: DElim -> TCM DTerm
evaluateElim (tm ::: _)                     = evaluateTerm tm
evaluateElim ((Dλ n b ::: DΠ _ vT _) :@: s) = evaluateTerm
                                              (b `subst` [n :~> (s ::: vT)])
evaluateElim (DRef n)                       = retrieveVar n
evaluateElim elim                           = fail ("evaluateElim: " <> show elim)

example :: (DTerm, DTerm)
example = (Dλ "a" (Dλ "x" (D_ "x")), DΠ "a" DStar (DΠ "x" (D_ "a") (D_ "x")))

runTCM :: TCM a -> Either String a
runTCM (TCM tcm) = evalStateT tcm mempty

main :: IO ()
main = do
  print $ runTCM (uncurry (∋) example)

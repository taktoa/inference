{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.State

import           Data.Hashable       (Hashable)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS

import           Data.Monoid
import           Data.String

import           Data.Text           (Text)
import qualified Data.Text           as T

import Numeric.Natural

import           GHC.Generics        (Generic)

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>
{-# INLINE (.>) #-}

(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) = (.)
infixr 9 <.
{-# INLINE (<.)#-}

(<#>) :: (Functor f) => f a -> (a -> b) -> f b
x <#> f = f <$> x
infixl 4 <#>
{-# INLINE (<#>) #-}

(#>) :: a -> (a -> b) -> b
(#>) = flip ($)
infixl 0 #>
{-# INLINE (#>) #-}

(<#) :: (a -> b) -> a -> b
(<#) = ($)
infixr 0 <#
{-# INLINE (<#) #-}

newtype Name = Name { fromName :: Text }
             deriving (Eq, Show, Read, Generic)

data InferenceError
  = UnboundVariableError
    { _context :: Int
    , _name    :: Name
    }
  | TypeSynthesisError
    {
    }

instance IsString Name where
  fromString = T.pack .> Name

data DTerm = DType
           | DConst Text
           | D_ DElim
           | DΠ Name DTerm DTerm
           | Dλ Name DTerm
           deriving (Eq, Show, Read, Generic)

instance IsString DTerm where
  fromString = T.pack .> DConst

data DElim = DTerm ::: DTerm
           | DElim :@: DTerm
           | DRef Name
           deriving (Eq, Show, Read, Generic)

instance IsString DElim where
  fromString = fromString .> DRef

data DConstraint = DTerm :≡: DTerm
                 deriving (Eq, Show, Read, Generic)

data Ctx = Ctx { _environment :: HashMap Name DTerm
               , _constraints :: HashSet DConstraint }
         deriving (Eq, Show, Read, Generic)

instance Hashable Name
instance Hashable DTerm
instance Hashable DElim
instance Hashable DConstraint

instance Monoid Ctx where
  mempty = Ctx mempty mempty
  mappend (Ctx m1 s1) (Ctx m2 s2) = Ctx (m1 <> m2) (s1 <> s2)

newtype TCM a = TCM (StateT Ctx (Either String) a)
              deriving ( Functor, Applicative, Alternative
                       , Monad, MonadPlus, MonadState Ctx )

data Replace = Name :~> DElim

subst :: DTerm -> [Replace] -> DTerm
subst term []                      = term
subst term ((name :~> value):rest) = subst (subTm name value term) rest
  where
    subTm :: Name -> DElim -> DTerm -> DTerm
    subTm _ _ DType                 = DType
    subTm _ _ (DConst s)            = DConst s
    subTm n r (D_ el)               = D_ $ subEl n r el
    subTm n r (DΠ v vT bT) | n /= v = let vT' = subTm n r vT
                                          bT' = subTm n r bT
                                      in DΠ v vT' bT'
    subTm n r (Dλ v b)     | n /= v = Dλ v $ subTm n r b
    subTm _ _ other                 = other

    subEl :: Name -> DElim -> DElim -> DElim
    subEl n r (tm ::: ty) = subTm n r tm ::: subTm n r ty
    subEl n r (el :@: ty) = subEl n r el :@: subTm n r ty
    subEl n r (DRef var)  | n == var = r
    subEl _ _ other       = other

tell :: (Monad m, Monoid s) => s -> StateT s m ()
tell s = modify (<> s)

with :: [(Name, DTerm)] -> TCM a -> TCM a
with bs m = TCM (tell (Ctx (HM.fromList bs) mempty)) >> m

liftMaybe :: String -> Maybe a -> TCM a
liftMaybe _ (Just x) = pure x
liftMaybe e Nothing  = fail e

retrieveVar :: Name -> TCM DTerm
retrieveVar n = let err = "Could not find variable: " <> show n
                in get >>= _environment .> HM.lookup n .> liftMaybe err

(∋) :: DTerm -> DTerm -> TCM ()
DType ∋ DType                    = pure ()
DType ∋ (DΠ v vT bT)             = DType ∋ vT >> with [(v, vT)] (DType ∋ bT)
(DΠ v vT bT) ∋ (Dλ n b) | v == n = with [(v, vT)] $ b ∋ bT
tm ∋ (D_ e)                      = do ty  <- inferType e
                                      tm' <- evaluateTerm tm
                                      ty' <- evaluateTerm ty
                                      guard $ tm' == ty'
tm ∋ other                       = fail $ show tm <> " ∌ " <> show other

inferType :: DElim -> TCM DTerm
inferType (tm ::: ty) = DType ∋ ty >> ty ∋ tm >> evaluateTerm ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           evaluateTerm $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = retrieveVar n >>= evaluateTerm

evaluateTerm :: DTerm -> TCM DTerm
evaluateTerm DType      = pure DType
evaluateTerm (DConst i) = pure $ DConst i
evaluateTerm (D_ el)    = evaluateElim el
evaluateTerm term       = fail (show term)

evaluateElim :: DElim -> TCM DTerm
evaluateElim (tm ::: _)                     = evaluateTerm tm
evaluateElim ((Dλ n b ::: DΠ _ vT _) :@: s) = evaluateTerm
                                              $ b `subst` [n :~> (s ::: vT)]
evaluateElim elim                           = fail (show elim)

runTCM :: TCM a -> Either String a
runTCM (TCM tcm) = evalStateT tcm mempty

main :: IO ()
main = pure ()

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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Hashable        (Hashable)

import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM

import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HS

import           Data.Semigroup
import           Data.String

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Numeric.Natural

import           GHC.Generics         (Generic)

import           Context
import           DCore
import           Evaluable
import           InferenceError
import           Name
import           Utils

class (Monad m, MonadThrow m, MonadState Context m) => MonadTC m

instance MonadTC TCM

lookupVar :: (MonadThrow m, MonadState Context m) => Name -> m DTerm
lookupVar n = do ctx <- get
                 case HM.lookup n (_environment ctx)
                   of Just v  -> pure v
                      Nothing -> throwUnboundVariableError ctx n

newtype TCM a = MkTCM (StateT Context (Either SomeException) a)
              deriving ( Functor, Applicative, Monad
                       , MonadState Context, MonadThrow )

instance Evaluable TCM DTerm where
  type Normal DTerm = DTerm
  eval = evaluateTerm

instance Evaluable TCM DElim where
  type Normal DElim = DTerm
  eval = evaluateElim

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

tell :: (Monad m, Semigroup s) => s -> StateT s m ()
tell s = modify (<> s)

withVars :: [(Name, DTerm)] -> TCM a -> TCM a
withVars bs m = MkTCM (tell (MkContext (HM.fromList bs) mempty)) >> m

liftMaybe :: String -> Maybe a -> TCM a
liftMaybe _ (Just x) = pure x
liftMaybe e Nothing  = fail e

(∋) :: DTerm -> DTerm -> TCM ()
DType ∋ DType                    = pure ()
DType ∋ (DΠ v vT bT)             = DType ∋ vT >> withVars [(v, vT)] (DType ∋ bT)
(DΠ v vT bT) ∋ (Dλ n b) | v == n = withVars [(v, vT)] $ b ∋ bT
ty ∋ (D_ e)                      = do inferred <- inferType e
                                      success <- ty === inferred
                                      unless success
                                        $ throwTypeSynthesisError ty (D_ e)
ty ∋ tm                          = throwTypeSynthesisError ty tm

inferType :: DElim -> TCM DTerm
inferType (tm ::: ty) = DType ∋ ty >> ty ∋ tm >> evaluateTerm ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           evaluateTerm $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = lookupVar n >>= evaluateTerm

evaluateTerm :: DTerm -> TCM DTerm
evaluateTerm DType      = pure DType
evaluateTerm (DConst i) = pure $ DConst i
evaluateTerm (D_ el)    = evaluateElim el
evaluateTerm term       = throwTermEvaluationError term

evaluateElim :: DElim -> TCM DTerm
evaluateElim (tm ::: _)                     = evaluateTerm tm
evaluateElim ((Dλ n b ::: DΠ _ vT _) :@: s) = evaluateTerm
                                              $ b `subst` [n :~> (s ::: vT)]
evaluateElim elim                           = throwElimEvaluationError elim

runTCM :: TCM a -> Either String a
runTCM (MkTCM tcm) = bimap show id $ evalStateT tcm mempty

main :: IO ()
main = pure ()

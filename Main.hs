{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Main (module Main, module Types, module Evaluable) where

import           Control.Monad.Except

import           Evaluable
import           Types
import           Utils

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
inferType (tm ::: ty) = DType ∋ ty >> ty ∋ tm >> evaluateTerm ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           evaluateTerm $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = lookupVar n >>= evaluateTerm

main :: IO ()
main = pure ()

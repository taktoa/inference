{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Main
  ( module Main
  , module Types
  , module Evaluable
  , module Utils
  ) where

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
inferType (tm ::: ty) = DType ∋ ty >> ty ∋ tm >> eval ty
inferType (el :@: tm) = do DΠ v vT bT <- inferType el
                           vT ∋ tm
                           eval $ bT `subst` [v :~> (tm ::: vT)]
inferType (DRef n)    = lookupVar n >>= eval

debug :: DElim
debug = (Dλ "x" (D_ "x")) ::: (DΠ "x" DType (D_ "x"))

main :: IO ()
main = pure ()

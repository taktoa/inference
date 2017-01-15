{-# LANGUAGE DeriveGeneric #-}

module InferenceError where

import           Control.Monad.Catch

import           Data.Semigroup

import           GHC.Generics        (Generic)

import           Context
import           DCore
import           Name
import           Utils

data InferenceError
  = UnboundVariableError
    { _context :: Context
    , _name    :: Name }
  | TypeSynthesisError
    { _type :: DTerm
    , _term :: DTerm }
  | ElimEvaluationError
    { _elim :: DElim }
  | TermEvaluationError
    { _term :: DTerm }
  deriving (Eq, Generic)

instance Show InferenceError where
  show (UnboundVariableError _ n) = "Could not find variable: " <> show n
  show (TypeSynthesisError ty tm) = show ty <> " ∌ " <> show tm
  show (ElimEvaluationError el)   = "Could not evaluate eliminator: " <> show el
  show (TermEvaluationError tm)   = "Could not evaluate term: " <> show tm

instance Exception InferenceError

throwUnboundVariableError :: (MonadThrow m) => Context -> Name -> m a
throwUnboundVariableError ctx name = UnboundVariableError ctx name |> throwM

throwTypeSynthesisError :: (MonadThrow m) => DTerm -> DTerm -> m a
throwTypeSynthesisError ty tm = TypeSynthesisError ty tm |> throwM

throwElimEvaluationError :: (MonadThrow m) => DElim -> m a
throwElimEvaluationError el = ElimEvaluationError el |> throwM

throwTermEvaluationError :: (MonadThrow m) => DTerm -> m a
throwTermEvaluationError tm = TermEvaluationError tm |> throwM

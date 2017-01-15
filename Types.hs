{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedLabels           #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE GADTs                      #-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- module Types
--   ( Name (..)
--   -- FIXME
--   ) where

module Types where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.Text            as T

import           Evaluable
import           Utils

--------------------------------------------------------------------------------
-- Exported: Name (..)

newtype Name = MkName { fromName :: Text }
             deriving (Eq, Show, Read, Generic, Hashable)

instance IsString Name where
  fromString = T.pack .> MkName

--------------------------------------------------------------------------------
-- Exported: DTerm (..)

data DTerm = DType
           | DConst Text
           | D_ DElim
           | DΠ Name DTerm DTerm
           | Dλ Name DTerm
           deriving (Eq, Show, Read, Generic)

instance Hashable DTerm

instance IsString DTerm where
  fromString = T.pack .> DConst

instance (MonadThrow m) => Evaluable m DTerm where
  type Normal DTerm = DTerm
  eval = evaluateTerm

evaluateTerm :: (MonadThrow m) => DTerm -> m DTerm
evaluateTerm DType      = pure DType
evaluateTerm (DConst i) = pure $ DConst i
evaluateTerm (D_ el)    = evaluateElim el
evaluateTerm term       = throwTermEvaluationError term

--------------------------------------------------------------------------------
-- Exported: DElim (..)

data DElim = DTerm ::: DTerm
           | DElim :@: DTerm
           | DRef Name
           deriving (Eq, Show, Read, Generic)

instance Hashable DElim

instance IsString DElim where
  fromString = fromString .> DRef

instance (MonadThrow m) => Evaluable m DElim where
  type Normal DElim = DTerm
  eval = evaluateElim

evaluateElim :: (MonadThrow m) => DElim -> m DTerm
evaluateElim (tm ::: _)                     = evaluateTerm tm
evaluateElim ((Dλ n b ::: DΠ _ vT _) :@: s) = evaluateTerm
                                              $ b `subst` [n :~> (s ::: vT)]
evaluateElim elim                           = throwElimEvaluationError elim

--------------------------------------------------------------------------------
-- Exported:
--     InferenceError
--   , throwUnboundVariableError
--   , throwTypeSynthesisError
--   , throwElimEvaluationError
--   , throwTermEvaluationError

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

instance Hashable InferenceError

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

--------------------------------------------------------------------------------
-- Exported: Replace (..), subst

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

--------------------------------------------------------------------------------
-- Exported: Binding (..), bindingToPair

data Binding = Name := DTerm

bindingToPair :: Binding -> (Name, DTerm)
bindingToPair (n := tm) = (n, tm)

bindingsToMap :: [Binding] -> HashMap Name DTerm
bindingsToMap = map bindingToPair .> HM.fromList

--------------------------------------------------------------------------------
-- Exported: MonadTC (..)

class (Monad m, MonadCatch m) => MonadTC m where
  runTC          :: Context -> m a -> Either String a
  addVar         :: Binding    -> m ()
  lookupVar      :: Name       -> m DTerm
  emitConstraint :: Constraint -> m ()
  getConstraints :: m [Constraint]

withVars :: (MonadTC m) => [Binding] -> m a -> m a
withVars bs m = mapM_ addVar bs >> m

--------------------------------------------------------------------------------
-- Exported: TCM

newtype TCM a = MkTCM (StateT Context (Either SomeException) a)
              deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

instance MonadTC TCM where
  runTC ctx (MkTCM tcm) = bimap show id $ evalStateT tcm ctx

  addVar b = let ctx = MkContext (HM.fromList [bindingToPair b]) mempty
             in MkTCM (modify (<> ctx))

  lookupVar n = do ctx <- MkTCM get
                   case HM.lookup n (_environment ctx)
                     of Just v  -> pure v
                        Nothing -> throwUnboundVariableError ctx n

  emitConstraint con = let ctx = MkContext mempty (HS.fromList [con])
                       in MkTCM (modify (<> ctx))

  getConstraints = MkTCM (get <#> _constraints .> HS.toList)

--------------------------------------------------------------------------------
-- Exported: Constraint (..)

data Constraint
  = DTerm :≤: DTerm
  deriving (Eq, Show, Read, Generic)

instance Hashable Constraint

--------------------------------------------------------------------------------
-- Exported: Context (..)

data Context
  = MkContext
    { _environment :: HashMap Name DTerm
    , _constraints :: HashSet Constraint }
  deriving (Eq, Show, Read, Generic)

instance Hashable Context

instance Semigroup Context where
  (MkContext m1 s1) <> (MkContext m2 s2) = MkContext (m2 <> m1) (s2 <> s1)

instance Monoid Context where
  mappend = (<>)
  mempty  = MkContext mempty mempty

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

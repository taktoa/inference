{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
  ( SupplyT, SupplyM, runSupply, runSupplyIO
  , getSupply, fresh, split
  ) where

import           Control.Category           ((>>>))
import           Control.Concurrent.Supply
import           Control.Monad.Identity
import           Control.Monad.State.Strict

newtype SupplyT m a
  = SupplyT (StateT Supply m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

type SupplyM a = SupplyT Identity a

runSupply :: (Monad m) => SupplyT m a -> Supply -> m a
runSupply (SupplyT m) = evalStateT m

runSupplyIO :: (MonadIO m) => SupplyT m a -> m a
runSupplyIO m = liftIO newSupply >>= runSupply m

getSupply :: (Monad m) => SupplyT m Supply
getSupply = SupplyT get

fresh :: (Monad m) => SupplyT m Int
fresh = SupplyT $ StateT (freshId >>> pure)

split :: (Monad m) => SupplyT m a -> SupplyT m b -> SupplyT m (a, b)
split mA mB = do
  (sA, sB) <- splitSupply <$> getSupply
  rA <- lift $ runSupply mA sA
  rB <- lift $ runSupply mB sB
  pure (rA, rB)

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Supply
  ( USupply, newUSupply, freshUSupply, splitUSupply
  , SupplyT, SupplyM, runSupply, runSupplyIO
  , getSupply, fresh, split
  ) where

import           Control.Arrow              ((***), (>>>))
import           Control.Concurrent.Supply
import           Control.Monad.Identity
import           Control.Monad.State.Strict

data USupply u
  = MkUSupply
    {-# UNPACK #-} !(Int -> u)
    {-# UNPACK #-} !Supply

newUSupply :: (Int -> u) -> IO (USupply u)
newUSupply conv = do
  supply <- newSupply
  pure $ MkUSupply conv supply

freshUSupply :: USupply u -> (u, USupply u)
freshUSupply (MkUSupply conv supply) = (conv *** MkUSupply conv)
                                       $ freshId supply

splitUSupply :: USupply u -> (USupply u, USupply u)
splitUSupply (MkUSupply conv supply) = let (sA, sB) = splitSupply supply
                                       in (MkUSupply conv sA, MkUSupply conv sB)

newtype SupplyT u m a
  = SupplyT (StateT (USupply u) m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

type SupplyM u a = SupplyT u Identity a

runSupply :: (Monad m) => SupplyT u m a -> USupply u -> m a
runSupply (SupplyT m) = evalStateT m

runSupplyIO :: (MonadIO m) => SupplyT u m a -> (Int -> u) -> m a
runSupplyIO m conv = liftIO (newUSupply conv) >>= runSupply m

getSupply :: (Monad m) => SupplyT u m (USupply u)
getSupply = SupplyT get

fresh :: (Monad m) => SupplyT u m u
fresh = SupplyT $ StateT (freshUSupply >>> pure)

split :: (Monad m) => SupplyT u m a -> SupplyT u m b -> SupplyT u m (a, b)
split mA mB = do
  (sA, sB) <- splitUSupply <$> getSupply
  rA <- lift $ runSupply mA sA
  rB <- lift $ runSupply mB sB
  pure (rA, rB)

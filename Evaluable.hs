{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Evaluable
  ( Evaluable (..)
  , (===)
  ) where

-- |
-- If a type 'a' has an instance of 'Evaluable', you can evaluate it to some
-- normal form 'Normal a' under a monad with the 'eval' function.
class (Monad m, Eq (Normal a)) => Evaluable m a where
  type Normal a

  eval :: a -> m (Normal a)

-- | Check if two values are equal under evaluation.
(===) :: (Evaluable m a) => a -> a -> m Bool
(===) x y = (==) <$> eval x <*> eval y

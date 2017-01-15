module Utils
  ( (<.), (.>)
  , (<|), (|>)
  , (<#>)
  , module Data.Bifunctor
  ) where

import           Data.Bifunctor

-- | Function composition.
(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) = (.)
infixr 9 <.
{-# INLINE (<.)#-}

-- | Flipped function composition.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>
{-# INLINE (.>) #-}

-- | Function application.
(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 0 <|
{-# INLINE (<|) #-}

-- | Flipped function application.
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>
{-# INLINE (|>) #-}

-- | Flipped 'fmap'.
(<#>) :: (Functor f) => f a -> (a -> b) -> f b
x <#> f = f <$> x
infixl 4 <#>
{-# INLINE (<#>) #-}

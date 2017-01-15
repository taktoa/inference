module Utils
  ( (<.), (.>), (<|), (|>), (<#>)
  , liftMaybe
  , compose
  , module Exported
  ) where

import           Data.HashMap.Strict as Exported (HashMap)
import           Data.HashSet        as Exported (HashSet)
import           Data.Text           as Exported (Text)

import           Data.Bifunctor      as Exported
import           Data.Semigroup      as Exported
import           Data.String         as Exported (IsString (..))

import           Data.Hashable       as Exported (Hashable)
import           GHC.Generics        as Exported (Generic)

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

-- | FIXME: doc
liftMaybe :: (Applicative f) => Maybe a -> f a -> f a
liftMaybe (Just x) _   = pure x
liftMaybe Nothing  err = err

-- | FIXME: doc
compose :: [a -> a] -> a -> a
compose = foldr (.>) id

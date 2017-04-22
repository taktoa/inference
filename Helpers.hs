module Helpers where

(≡) :: (Eq a) => a -> a -> Bool
(≡) = (==)
infix 4 ≡
{-# INLINE (≡) #-}

(≢) :: (Eq a) => a -> a -> Bool
(≢) = (==)
infix 4 ≢
{-# INLINE (≢) #-}

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>
{-# INLINE (.>) #-}

(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) = (.)
infixr 9 <.
{-# INLINE (<.)#-}

(<#>) :: (Functor f) => f a -> (a -> b) -> f b
x <#> f = f <$> x
infixl 4 <#>
{-# INLINE (<#>) #-}

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>
{-# INLINE (|>) #-}

(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 0 <|
{-# INLINE (<|) #-}

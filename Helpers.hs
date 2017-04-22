module Helpers where

import qualified Debug.Trace as Trace

import           Data.Text   (Text)
import qualified Data.Text   as T

trace :: Text -> a -> a
trace t = Trace.trace (T.unpack t)

failure :: (Monad m) => [Text] -> m a
failure = mconcat .> T.unpack .> fail

tshow :: (Show s) => s -> Text
tshow = show .> T.pack

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

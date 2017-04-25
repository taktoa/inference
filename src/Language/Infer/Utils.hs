{-# LANGUAGE OverloadedStrings #-}

module Language.Infer.Utils
  ( module Language.Infer.Utils
  , module Exported
  ) where

import           Data.Text           as T

import           Control.Applicative as Exported
import           Control.Arrow       as Exported
import           Control.Monad       as Exported
import           Data.Either         as Exported
import           Data.Foldable       as Exported (asum)
import           Data.Maybe          as Exported
import           Data.Monoid         as Exported

import           Data.Text           as Exported (Text)

import           GHC.Generics        as Exported (Generic)
import           GHC.Stack           as Exported (HasCallStack)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>
{-# INLINE (|>) #-}

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixl 9 .>
{-# INLINE (.>) #-}

(<#>) :: (Functor f) => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
infixr 4 <#>

fromSingletonList :: [a] -> Maybe a
fromSingletonList [x] = Just x
fromSingletonList _   = Nothing

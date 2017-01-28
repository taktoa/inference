{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Utils
  ( (<.), (.>), (<|), (|>), (<#>)
  , liftMaybe
  , compose
  , readEither, runReadS, runReadP
  , tshow, tread, treadM, treadE
  , SGR, sgr, sgrReset, sgrWrap
  , pattern SGRReset, pattern SGRBold
  , pattern SGRRed,   pattern SGRGreen,   pattern SGRBlue
  , pattern SGRCyan,  pattern SGRMagenta, pattern SGRYellow
  , module Exported
  ) where

import           Data.HashMap.Strict          as Exported (HashMap)
import           Data.HashSet                 as Exported (HashSet)
import           Data.Text                    as Exported (Text)

import           Data.Bifunctor               as Exported
import           Data.Either                  as Exported
import           Data.Maybe                   as Exported
import           Data.Semigroup               as Exported
import           Data.String                  as Exported (IsString (..))

import           Data.Hashable                as Exported (Hashable)
import           GHC.Generics                 as Exported (Generic)

import           Safe                         as Exported

import           Control.Lens.Cons            as Exported hiding ((<|), (|>))

import qualified Data.Text                    as T

import           Text.ParserCombinators.ReadP

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

-- | FIXME: doc
readEither :: (Read a) => String -> Either String a
readEither = runReadS reads

-- | FIXME: doc
runReadS :: ReadS a -> String -> Either String a
runReadS rs s = case [x | (x, t) <- rs s, ("", "") <- lex t]
                of [x] -> Right x
                   []  -> Left $ "no parse on " ++ prefix
                   _   -> Left $ "ambiguous parse on " ++ prefix
  where
    maxLength = 15
    prefix = let (a, b) = splitAt (maxLength - 3) s
             in '\"' : a ++ if length s <= maxLength then b ++ "\"" else "...\""

-- | FIXME: doc
runReadP :: ReadP a -> String -> Either String a
runReadP rp = runReadS (readP_to_S rp)

-- | FIXME: doc
tshow :: (Show s) => s -> Text
tshow = show .> T.pack

-- | FIXME: doc
tread :: (Read r) => Text -> r
tread = T.unpack .> read

-- | FIXME: doc
treadM :: (Read r) => Text -> Maybe r
treadM = T.unpack .> readMay

-- | FIXME: doc
treadE :: (Read r) => Text -> Either Text r
treadE = T.unpack .> readEither .> bimap T.pack id

-- | FIXME: doc
sgr :: SGR -> Text
sgr n = "\ESC[" <> tshow n <> "m"

-- | FIXME: doc
sgrReset :: Text
sgrReset = sgr 0

-- | FIXME: doc
sgrWrap :: SGR -> Text -> Text
sgrWrap n t = sgr n <> t <> sgr 0

type SGR = Int

pattern SGRReset   = 0
pattern SGRBold    = 1
pattern SGRRed     = 31
pattern SGRGreen   = 32
pattern SGRYellow  = 33
pattern SGRBlue    = 34
pattern SGRMagenta = 35
pattern SGRCyan    = 36

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative

import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO            as T

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS

import           Data.Aeson
import           Data.Aeson.BetterErrors
                 (Parse, ParseError, ParseError', asIntegral, asString,
                 displayError, displayError', eachInArray, eachInObject, key,
                 keyMay, keyOrDefault, nth, nthMay, nthOrDefault, parse,
                 toAesonParser, toAesonParser', withString)

import           Utils

data Person = Person String Int deriving (Show)

asPerson :: Parse e Person
asPerson = Person <$> key "name" asString <*> key "age" asIntegral

asPerson' :: Parse e Person
asPerson' = Person <$> nth 0 asString <*> nth 1 asIntegral

examples' :: [(Parse e Person, Text)]
examples' = [ (asPerson,  "{\"name\": \"Bob\", \"age\": 25}")
            , (asPerson', "[\"Angela\", 43]")
            , (asPerson,  "{\"name\": \"Bob\"}")
            , (asPerson,  "{\"name\": \"Bob\", \"age\": 25.1}")
            , (asPerson,  "[\"Bob\", 25]")
            ]

printErr :: (Show v) => Either ParseError' v -> IO ()
printErr = go .> mconcat .> T.putStr
  where
    go (Right val)    = [ "[", sgrWrap SGRGreen "SUCCESS", "]: "
                        , withNL (tshow val)
                        ]
    go (Left  err)    = [ "[", sgrWrap SGRRed "FAILURE", "]: " ]
                        <> case fromMaybe ("", []) (uncons (displayError' err))
                           of (x, xs) -> withNL x : map (indent 11 .> withNL) xs
    indent k t = T.replicate k " " <> t
    withNL t = t <> "\n"

examplesParsed :: [Either (ParseError err) Person]
examplesParsed = examples'
                 |> map (bimap id (T.encodeUtf8 .> LBS.fromStrict))
                 |> map (uncurry parse)

examples :: IO ()
examples = examplesParsed |> mapM_ printErr

main :: IO ()
main = pure ()

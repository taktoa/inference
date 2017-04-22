{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PrettyJSON () where

import           Text.PrettyPrint.ANSI.Leijen hiding (text, (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
-- import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..))

import           Data.Scientific              (Scientific)
import qualified Data.Scientific              as Sci

import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V

import           Data.HashMap.Lazy            (HashMap)
import qualified Data.HashMap.Lazy            as LHM

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Data.Aeson

import           Utils

text :: Text -> Doc
text = T.unpack .> PP.text

mapInitLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInitLast _ _ []     = []
mapInitLast _ g [x]    = [g x]
mapInitLast f g (x:xs) = f x : mapInitLast f g xs

instance Semigroup Doc where (<>) = mappend

instance Pretty Value where
  pretty = \case (Object   obj) -> obj |> LHM.toList |> mapP
                 (Array    arr) -> arr |> V.toList |> listP
                 (String   str) -> str |> text |> dquotes |> red
                 (Number   sci) -> tshow sci |> text |> yellow
                 (Bool   False) -> bold "false"
                 (Bool    True) -> bold "true"
                 Null           -> bold "null"
    where
      mapP :: (Pretty v) => [(Text, v)] -> Doc
      mapP m = let mp      = m |> fmap (second pretty)
                   padding = map fst mp |> map T.length |> maximum |> min 32
                   keyP    = text .> fill padding
                   pairP (k, v) = [keyP k, space, arrow, space, v]
                                  |> mconcat
               in mp |> map pairP |> vcat

      listP :: (Pretty v) => [v] -> Doc
      listP []     = text "[]"
      listP (x:xs) = let itemP i = comma <> space <> i
                     in [ [ lbracket, space, pretty x ]
                        , map (pretty .> itemP) xs
                        , [ rbracket ]
                        ] |> mconcat |> vsep |> align

      arrow = text "→"

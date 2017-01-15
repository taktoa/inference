{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Name
  ( Name (..)
  ) where

import           Data.Hashable (Hashable)
import           Data.String   (IsString (..))
import           Data.Text     (Text, pack)
import           GHC.Generics  (Generic)

import           Utils

newtype Name = Name { fromName :: Text }
             deriving (Eq, Show, Read, Generic, Hashable)

instance IsString Name where
  fromString = pack .> Name

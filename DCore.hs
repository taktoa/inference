{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module DCore where

import           Data.Hashable (Hashable)
import           Data.String   (IsString (..))
import           Data.Text     (Text, pack)
import           GHC.Generics  (Generic)

import           Name
import           Utils

data DElim = DTerm ::: DTerm
           | DElim :@: DTerm
           | DRef Name
           deriving (Eq, Show, Read, Generic, Hashable)

instance IsString DElim where
  fromString = fromString .> DRef

data DTerm = DType
           | DConst Text
           | D_ DElim
           | DΠ Name DTerm DTerm
           | Dλ Name DTerm
           deriving (Eq, Show, Read, Generic, Hashable)

instance IsString DTerm where
  fromString = pack .> DConst

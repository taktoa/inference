{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Context where

import           Data.Semigroup

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           GHC.Generics        (Generic)

import           DCore
import           Name

data DConstraint
  = DTerm :≤: DTerm
  deriving (Eq, Show, Read, Generic, Hashable)

data Context
  = MkContext
    { _environment :: HashMap Name DTerm
    , _constraints :: HashSet DConstraint }
  deriving (Eq, Show, Read, Generic)

instance Semigroup Context where
  (MkContext m1 s1) <> (MkContext m2 s2) = MkContext (m2 <> m1) (s2 <> s1)

instance Monoid Context where
  mappend = (<>)
  mempty  = MkContext mempty mempty

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CompressedJSON
  ( module CompressedJSON
  ) where

import           Control.Monad
import           Control.Monad.Reader   (Reader, ask, runReader)
import           Control.Monad.State    (State, runState, state)

import           Data.Vector            (Vector)
import qualified Data.Vector            as V

import           Data.Scientific        (Scientific)
import qualified Data.Scientific        as Sci

import qualified Data.HashMap.Lazy      as LHM

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS

import           Data.Binary            (Binary (..))
import qualified Data.Binary            as Binary

import           Data.Text              (Text)

import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IMap

import qualified Data.Foldable          as Seq (toList)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq

import           Control.Lens           hiding ((.>), (<.), (<|), (|>))

import qualified Data.Aeson             as A
import qualified Data.Aeson.Lens        as Lens

import qualified Codec.Compression.GZip as GZ

import           Data.Word

import           Utils

decompressJSON :: CompressedJSON -> A.Value
decompressJSON (MkCompressedJSON {..})
  = runDecompressM _dict $ decompressValue _json

compressJSON :: A.Value -> CompressedJSON
compressJSON = compressValue .> runCompressM .> uncurry MkCompressedJSON

-- instance Lens.AsNumber CompressedJSON where
--   _Number  = undefined :: Prism' CompressedJSON Scientific
--   _Double  = undefined :: Prism' CompressedJSON Double
--   _Integer = undefined :: Prism' CompressedJSON Integer
--
-- instance Lens.AsPrimitive CompressedJSON where
--   _Primitive = prism' fromPrimitive _
--     where
--       fromPrimitive :: Lens.Primitive -> CompressedJSON
--       fromPrimitive = _
--   _String    = undefined

data CompressedJSON
  = MkCompressedJSON
    { _dict :: {-# UNPACK #-} !CVDict
    , _json :: {-# UNPACK #-} !CValue
    }
  deriving (Show, Generic)

instance Binary CompressedJSON

cvdLookupString :: CVDict -> StringId -> Maybe Text
cvdLookupString (MkCVDict strs _) (MkStringId n) = V.indexM strs n

cvdLookupObject :: CVDict -> ObjectId -> Maybe (Vector Key)
cvdLookupObject (MkCVDict _ objs) (MkObjectId n) = V.indexM objs n

decompressValue :: CValue -> DecompressM A.Value
decompressValue = go
  where
    go :: CValue -> DecompressM A.Value
    go CVNull           = pure A.Null
    go CVBooleanFalse   = pure $ A.Bool False
    go CVBooleanTrue    = pure $ A.Bool True
    go (CVFloat    flt) = pure $ A.Number flt
    go (CVInteger  int) = pure $ A.Number (fromIntegral int)
    go (CVString   sid) = A.String <$> lookupStr sid
    go (CVArray    arr) = arr |> decompressList |> fmap A.Array
    go (CVObject i obj) = mkObject <$> lookupObj i <*> decompressList obj

    mkObject :: Vector Key -> Vector A.Value -> A.Value
    mkObject ks vs = V.zip (fromKey <$> ks) vs
                     |> V.toList |> LHM.fromList |> A.Object

    decompressList :: Vector CValue -> DecompressM (Vector A.Value)
    decompressList = V.toList .> mapM go .> fmap V.fromList

compressValue :: A.Value -> CompressM CValue
compressValue = go
  where
    go :: A.Value -> CompressM CValue
    go A.Null         = pure CVNull
    go (A.Bool False) = pure CVBooleanFalse
    go (A.Bool  True) = pure CVBooleanTrue
    go (A.Number sci) = pure $ scientificToCV sci
    go (A.String str) = CVString <$> internString str
    go (A.Array  arr) = V.toList arr |> compressList |> fmap CVArray
    go (A.Object obj) = let (ks, vs) = unzip (first MkKey <$> LHM.toList obj)
                        in CVObject <$> internObject ks <*> compressList vs

    compressList :: [A.Value] -> CompressM (Vector CValue)
    compressList = mapM go .> fmap V.fromList

    scientificToCV :: Scientific -> CValue
    scientificToCV sci = fromMaybe (CVFloat sci) $ cvInteger sci

    cvInteger :: Scientific -> Maybe CValue
    cvInteger sci = doubleOrInteger sci
                    |> either (const Nothing) integerToInt
                    |> fmap CVInteger

    doubleOrInteger :: Scientific -> Either Double Integer
    doubleOrInteger = Sci.floatingOrInteger

    integerToInt :: Integer -> Maybe Int
    integerToInt i = do guard $ i < fromIntegral (maxBound :: Int)
                        guard $ i > fromIntegral (minBound :: Int)
                        pure $ fromIntegral i

newtype DecompressM a
  = MkDecompressM (Reader CVDict a)
  deriving (Functor, Applicative, Monad)

runDecompressM :: CVDict -> DecompressM a -> a
runDecompressM dict (MkDecompressM m) = runReader m dict

lookupStr :: StringId -> DecompressM Text
lookupStr (MkStringId sid) = MkDecompressM $ do (MkCVDict strs _) <- ask
                                                V.indexM strs sid


lookupObj :: ObjectId -> DecompressM (Vector Key)
lookupObj (MkObjectId oid) = MkDecompressM $ do (MkCVDict _ objs) <- ask
                                                V.indexM objs oid

newtype CompressM a
  = MkCompressM (State Dict a)
  deriving (Functor, Applicative, Monad)

runCompressM :: CompressM a -> (CVDict, a)
runCompressM (MkCompressM m) = runState m (MkDict mempty mempty)
                               |> (\(x, dict) -> (cvdFromDict dict, x))

internString :: Text -> CompressM StringId
internString str = MkCompressM $ state (addStringToDict str)

internObject :: [Key] -> CompressM ObjectId
internObject ks = MkCompressM $ state (addObjectToDict (V.fromList ks))

data Dict
  = MkDict
    { _dictStrings :: Seq Text
    , _dictObjects :: Seq (Vector Key)
    }

addStringToDict :: Text -> Dict -> (StringId, Dict)
addStringToDict str (MkDict strs objs)
  = (MkStringId (Seq.length strs), MkDict (strs :> str) objs)

addObjectToDict :: Vector Key -> Dict -> (ObjectId, Dict)
addObjectToDict keys (MkDict strs objs)
  = (MkObjectId (Seq.length objs), MkDict strs (objs :> keys))

data CVDict
  = MkCVDict
    { _cvdStrings :: {-# UNPACK #-} !(Vector Text)
    , _cvdObjects :: {-# UNPACK #-} !(Vector (Vector Key))
    }
  deriving (Show, Generic)

instance Binary CVDict

-- instance Binary CVDict where
--   put (MkCVDict strs objs)
--     = let strsB = GZ.compress $ Binary.encode strs
--           objsB = GZ.compress $ Binary.encode objs
--       in put strsB >> put objsB
--   get = MkCVDict
--         <$> (get <#> GZ.decompress .> Binary.decode)
--         <*> (get <#> GZ.decompress .> Binary.decode)

cvdFromDict :: Dict -> CVDict
cvdFromDict (MkDict strs objs) = let seqToV = Seq.toList .> V.fromList
                                 in MkCVDict (seqToV strs) (seqToV objs)

data CValue
  = CVNull
  | CVBooleanFalse
  | CVBooleanTrue
  | CVFloat
    {-# UNPACK #-} !Scientific
  | CVInteger
    {-# UNPACK #-} !Int
  | CVString
    {-# UNPACK #-} !StringId
  | CVArray
    {-# UNPACK #-} !(Vector CValue)
  | CVObject
    {-# UNPACK #-} !ObjectId
    {-# UNPACK #-} !(Vector CValue)
  | CVCompressed
    CompressedJSON
  deriving (Show, Generic)

instance Binary CValue where
  put CVNull            = put (0 :: Word8)
  put CVBooleanFalse    = put (1 :: Word8)
  put CVBooleanTrue     = put (2 :: Word8)
  put (CVFloat     flt) = put (3 :: Word8) >> put flt
  put (CVInteger   int) = put (4 :: Word8) >> put int
  put (CVString    str) = put (5 :: Word8) >> put str
  put (CVArray     arr) = put (6 :: Word8) >> put arr
  put (CVObject  i vec) = put (7 :: Word8) >> put i >> put vec
  put (CVCompressed cj) = put (8 :: Word8) >> put cj
  get = get >>= go
    where
      go :: Word32 -> Binary.Get CValue
      go 0 = pure CVNull
      go 1 = pure CVBooleanFalse
      go 2 = pure CVBooleanTrue
      go 3 = get <#> CVFloat
      go 4 = get <#> CVInteger
      go 5 = get <#> CVString
      go 6 = get <#> CVArray
      go 7 = get <#> uncurry CVObject
      go 8 = get <#> CVCompressed
      go _ = fail "Failure while decoding CValue: invalid tag number"

newtype StringId
  = MkStringId Int
  deriving (Eq, Show, Generic, Binary)

newtype ObjectId
  = MkObjectId Int
  deriving (Eq, Show, Generic, Binary)

newtype Key
  = MkKey { fromKey :: Text }
  deriving (Eq, Show, Generic, Binary)

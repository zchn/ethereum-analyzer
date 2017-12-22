{-|
  This module defines various utility functions used across the
  Network.Haskoin modules.
-}
module Legacy.Haskoin.V0102.Network.Haskoin.Util
  ( toStrictBS
  , toLazyBS
  , stringToBS
  , bsToString
  , bsToInteger
  , integerToBS
  , bsToHex
  , hexToBS
  , encode'
  , decode'
  , runPut'
  , runGet'
  , decodeOrFail'
  , runGetOrFail'
  , fromDecode
  , fromRunGet
  , decodeToEither
  , decodeToMaybe
  , isolate
  ) where

import Control.Monad (guard)

import Numeric (readHex)

import Data.Binary (Binary, decode, decodeOrFail, encode)
import Data.Binary.Get
       (ByteOffset, Get, getByteString, runGet, runGetOrFail)
import Data.Binary.Put (Put, runPut)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.List (unfoldr)
import Data.List.Split (chunksOf)

import Data.Word (Word8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL

-- ByteString helpers
-- | Transforms a lazy bytestring into a strict bytestring
toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

-- | Transforms a strict bytestring into a lazy bytestring
toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]

-- | Transforms a string into a strict bytestring
stringToBS :: String -> BS.ByteString
stringToBS = C.pack

-- | Transform a strict bytestring to a string
bsToString :: BS.ByteString -> String
bsToString = C.unpack

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = foldr f 0 . reverse . BS.unpack
  where
    f w n = toInteger w .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
  | i > 0 = BS.pack $ reverse $ unfoldr f i
  | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)

-- | Encode a bytestring to a base16 (HEX) representation
bsToHex :: BS.ByteString -> String
bsToHex = bsToString . toStrictBS . BSB.toLazyByteString . BSB.byteStringHex

-- | Decode a base16 (HEX) string from a bytestring. This function can fail
-- if the string contains invalid HEX characters
hexToBS :: String -> Maybe BS.ByteString
hexToBS xs = BS.pack <$> mapM hexWord (chunksOf 2 xs)
  where
    hexWord x = do
      guard $ length x == 2
      let hs = readHex x
      guard $ not $ null hs
      let [(w, s)] = hs
      guard $ null s
      return w

-- Data.Binary helpers
-- | Strict version of @Data.Binary.encode@
encode' :: Binary a => a -> BS.ByteString
encode' = toStrictBS . encode

-- | Strict version of @Data.Binary.decode@
decode' :: Binary a => BS.ByteString -> a
decode' = decode . toLazyBS

-- | Strict version of @Data.Binary.runGet@
runGet' :: Binary a => Get a -> BS.ByteString -> a
runGet' m = runGet m . toLazyBS

-- | Strict version of @Data.Binary.runPut@
runPut' :: Put -> BS.ByteString
runPut' = toStrictBS . runPut

-- | Strict version of @Data.Binary.decodeOrFail@
decodeOrFail' ::
     Binary a
  => BS.ByteString
  -> Either (BS.ByteString, ByteOffset, String) (BS.ByteString, ByteOffset, a)
decodeOrFail' bs =
  case decodeOrFail $ toLazyBS bs of
    Left (lbs, o, err) -> Left (toStrictBS lbs, o, err)
    Right (lbs, o, res) -> Right (toStrictBS lbs, o, res)

-- | Strict version of @Data.Binary.runGetOrFail@
runGetOrFail' ::
     Binary a
  => Get a
  -> BS.ByteString
  -> Either (BS.ByteString, ByteOffset, String) (BS.ByteString, ByteOffset, a)
runGetOrFail' m bs =
  case runGetOrFail m $ toLazyBS bs of
    Left (lbs, o, err) -> Left (toStrictBS lbs, o, err)
    Right (lbs, o, res) -> Right (toStrictBS lbs, o, res)

-- | Try to decode a Data.Binary value. If decoding succeeds, apply the function
-- to the result. Otherwise, return the default value.
fromDecode ::
     Binary a
  => BS.ByteString -- ^ The bytestring to decode
  -> b -- ^ Default value to return when decoding fails
  -> (a -> b) -- ^ Function to apply when decoding succeeds
  -> b -- ^ Final result
fromDecode bs def f = either (const def) (f . lst) $ decodeOrFail' bs
  where
    lst (_, _, c) = c

-- | Try to run a Data.Binary.Get monad. If decoding succeeds, apply a function
-- to the result. Otherwise, return the default value.
fromRunGet ::
     Binary a
  => Get a -- ^ The Get monad to run
  -> BS.ByteString -- ^ The bytestring to decode
  -> b -- ^ Default value to return when decoding fails
  -> (a -> b) -- ^ Function to apply when decoding succeeds
  -> b -- ^ Final result
fromRunGet m bs def f = either (const def) (f . lst) $ runGetOrFail' m bs
  where
    lst (_, _, c) = c

-- | Decode a Data.Binary value into the Either monad. A Right value is returned
-- with the result upon success. Otherwise a Left value with the error message
-- is returned.
decodeToEither :: Binary a => BS.ByteString -> Either String a
decodeToEither bs =
  case decodeOrFail' bs of
    Left (_, _, err) -> Left err
    Right (_, _, res) -> Right res

-- | Decode a Data.Binary value into the Maybe monad. A Just value is returned
-- with the result upon success. Otherwise, Nothing is returned.
decodeToMaybe :: Binary a => BS.ByteString -> Maybe a
decodeToMaybe bs = fromDecode bs Nothing Just

-- | Isolate a Data.Binary.Get monad for the next @Int@ bytes. Only the next
-- @Int@ bytes of the input bytestring will be available for the Get monad to
-- consume. This function will fail if the Get monad fails or some of the input
-- is not consumed.
isolate :: Binary a => Int -> Get a -> Get a
isolate i g = do
  bs <- getByteString i
  case runGetOrFail' g bs of
    Left (_, _, err) -> fail err
    Right (unconsumed, _, res)
      | BS.null unconsumed -> return res
      | otherwise -> fail "Isolate: unconsumed input"

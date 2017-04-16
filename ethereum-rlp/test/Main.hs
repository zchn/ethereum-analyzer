{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.Functor
import Data.List
import Data.Monoid
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Blockchain.Data.RLP

testNumber::Assertion
testNumber = do
  let n = 20::Integer
  assertEqual "rlp encoding failed for small number" (rlpDecode $ rlpDeserialize $ rlpSerialize $ rlpEncode n) n

testNumberOutput :: Assertion
testNumberOutput = do
  let n = 25 :: Integer
  assertEqual "rlp encoding output failed for small number" (rlpEncode n) (RLPScalar 25)

testNumberSerialize :: Assertion
testNumberSerialize = do
  let n = 75 :: Integer
  assertEqual "rlp encoding output failed for small number" (rlpSerialize $ rlpEncode n) "K"

testBigNumberSerialize :: Assertion
testBigNumberSerialize = do
  let n = 1 `shiftL` 250 :: Integer
  assertEqual "rlp encoding output failed for small number" (rlpSerialize $ rlpEncode n)
    "\160\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"

testStringSerialize :: Assertion
testStringSerialize = do
  let s = "this is a string" :: ByteString
  assertEqual "rlp serialization failed for string" (rlpSerialize $ rlpEncode s) "\144this is a string"

main::IO ()
main =
  defaultMainWithOpts
  [
   testCase "test RLP number encoding" testNumber,
   testCase "test RLP number encoding output" testNumberOutput,
   testCase "test RLP number serialization" testNumberSerialize,
   testCase "test RLP big number serialization" testBigNumberSerialize,
   testCase "test RLP string serialization" testStringSerialize
  ] mempty

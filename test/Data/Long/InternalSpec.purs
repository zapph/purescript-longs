module Data.Long.InternalSpec
       ( internalSpec
       ) where

import Prelude

import Data.Int (binary, decimal, hexadecimal, octal)
import Data.Long.FFI as FFI
import Data.Long.Internal (safeReadLong)
import Data.Long.TestUtils (i2lS, isSignedV, isUnsignedV)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

internalSpec :: Spec Unit
internalSpec = describe "safeReadLong" do
  it "should leave valid strings unchanged" do
    safeReadLong "12345" isSignedV decimal `shouldEqual` Just (i2lS 12345)
    safeReadLong "+12345" isSignedV decimal `shouldEqual` Just (i2lS 12345)
    safeReadLong "-12345" isSignedV decimal `shouldEqual` Just (i2lS (-12345))
    safeReadLong "-12345" isSignedV decimal `shouldEqual` Just (i2lS (-12345))

  it "should read signed zeros" do
    safeReadLong "0" isSignedV decimal `shouldEqual` Just FFI.zero
    safeReadLong "-0" isSignedV decimal `shouldEqual` Just FFI.zero
    safeReadLong "000" isSignedV decimal `shouldEqual` Just FFI.zero
    safeReadLong "-00" isSignedV decimal `shouldEqual` Just FFI.zero

  it "should read unsigned zeros" do
    safeReadLong "0" isUnsignedV decimal `shouldEqual` Just FFI.uzero
    safeReadLong "-0" isUnsignedV decimal `shouldEqual` Just FFI.uzero
    safeReadLong "000" isUnsignedV decimal `shouldEqual` Just FFI.uzero
    safeReadLong "-00" isUnsignedV decimal `shouldEqual` Just FFI.uzero

  it "should return Nothing on empty string" do
    safeReadLong "" isSignedV decimal `shouldEqual` Nothing

  it "should disallow negative for unsigned" do
    safeReadLong "-123" isUnsignedV decimal `shouldEqual` Nothing

  it "should disallow invalid characters depending on radix" do
    safeReadLong "1010" isSignedV binary `shouldSatisfy` isJust
    safeReadLong "-1010" isSignedV binary `shouldSatisfy` isJust
    safeReadLong "1012" isSignedV binary `shouldSatisfy` isNothing

    safeReadLong "1234" isSignedV octal `shouldSatisfy` isJust
    safeReadLong "1834" isSignedV octal `shouldSatisfy` isNothing

    safeReadLong "1bcd" isSignedV hexadecimal `shouldSatisfy` isJust
    safeReadLong "1BCd" isSignedV hexadecimal `shouldSatisfy` isJust
    safeReadLong "1bcz" isSignedV hexadecimal `shouldSatisfy` isNothing

  it "should read at the limits" do
    safeReadLong "9223372036854775807" isSignedV decimal `shouldEqual`
      Just FFI.maxValue
    safeReadLong "+009223372036854775807" isSignedV decimal `shouldEqual`
      Just FFI.maxValue
    safeReadLong "-09223372036854775808" isSignedV decimal `shouldEqual`
      Just FFI.minValue

    safeReadLong "18446744073709551615" isUnsignedV decimal `shouldEqual`
      Just FFI.maxUnsignedValue

    safeReadLong "7fffffffffffffff" isSignedV hexadecimal `shouldEqual`
      Just FFI.maxValue
    safeReadLong "-8000000000000000" isSignedV hexadecimal `shouldEqual`
      Just FFI.minValue

  it "should fail for overflows" do
    safeReadLong "9223372036854775808" isSignedV decimal `shouldSatisfy` isNothing
    safeReadLong "-9223372036854775809" isSignedV decimal `shouldSatisfy` isNothing

    safeReadLong "18446744073709551616" isUnsignedV decimal `shouldSatisfy` isNothing

    safeReadLong "8000000000000000" isSignedV hexadecimal `shouldSatisfy` isNothing
    safeReadLong "-8000000000000001" isSignedV hexadecimal `shouldSatisfy` isNothing

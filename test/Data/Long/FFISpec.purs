module Data.Long.FFISpec
       ( ffiSpec
       ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Long.FFI (IsLittleEndian(..), IsUnsigned(..), Long, Radix(..))
import Data.Long.FFI as FFI
import Data.Long.Internal as Internal
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Test.Assert (assert)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

ffiSpec :: Spec Unit
ffiSpec = describe "FFI" do
  it "should have correct values for constants" do
    FFI.zero `shouldEqual` (fromIntS 0)
    FFI.one `shouldEqual` (fromIntS 1)
    FFI.negOne `shouldEqual` (fromIntS (-1))
    FFI.uzero `shouldEqual` (fromIntU 0)
    FFI.uone `shouldEqual` (fromIntU 1)
    FFI.maxValue `shouldEqual` (fromStringS "9223372036854775807")
    FFI.minValue `shouldEqual` (fromStringS "-9223372036854775808")
    FFI.maxUnsignedValue `shouldEqual` (fromStringU "18446744073709551615")

  it "should create longs" do
    liftEffect $ assert $ FFI.isLong (unsafeToForeign FFI.zero)
    (runFn3 FFI.fromBits sampleS.low sampleS.high isSignedV) `shouldEqual` sampleS.value
    (runFn3 FFI.fromBits sampleU.low sampleU.high isUnsignedV) `shouldEqual` sampleU.value
    (runFn3 FFI.fromBytes sampleS.beBytes isSignedV isBigEndianV) `shouldEqual` sampleS.value
    (runFn2 FFI.fromBytesLE sampleS.leBytes isSignedV) `shouldEqual` sampleS.value
    (runFn2 FFI.fromBytesBE sampleS.beBytes isSignedV) `shouldEqual` sampleS.value
    (runFn2 FFI.fromInt 2 isSignedV) `shouldEqual` (fromStringS "2")
    (runFn2 FFI.fromNumber 2.0 isSignedV) `shouldEqual` (fromStringS "2")

  it "should access fields" do
    FFI.unsigned (fromStringU "2") `shouldEqual` isUnsignedV

  it "should access methods" do
    (FFI.add (fromStringS "2") (fromStringS "3")) `shouldEqual` (fromStringS "5")
    (FFI.and (fromStringS "2") (fromStringS "1")) `shouldEqual` (fromStringS "0")

    (FFI.compare (fromStringS "1") (fromStringS "2")) `shouldEqual` (-1)
    (FFI.compare (fromStringS "2") (fromStringS "2")) `shouldEqual` 0
    (FFI.compare (fromStringS "2") (fromStringS "1")) `shouldEqual` 1

    (FFI.divide (fromStringS "8") (fromStringS "3")) `shouldEqual` (fromStringS "2")
    (Internal.numberBitsToInt $ FFI.getHighBits sampleS.value) `shouldEqual` sampleS.high
    (Internal.numberBitsToInt $ FFI.getHighBitsUnsigned sampleU.value) `shouldEqual` sampleU.high
    (Internal.numberBitsToInt $ FFI.getLowBits sampleS.value) `shouldEqual` sampleS.low
    (Internal.numberBitsToInt $ FFI.getLowBitsUnsigned sampleU.value) `shouldEqual` sampleU.low
    (fromStringS "5") `shouldSatisfy` (_ `FFI.greaterThan` (fromStringS "2"))
    (fromStringS "5") `shouldSatisfy` (_ `FFI.greaterThanOrEqual` (fromStringS "5"))
    (fromStringS "6") `shouldSatisfy` FFI.isEven
    (fromStringS "-6") `shouldSatisfy` FFI.isNegative
    (fromStringS "5") `shouldSatisfy` FFI.isOdd
    (fromStringS "5") `shouldSatisfy` FFI.isPositive
    FFI.zero `shouldSatisfy` FFI.isZero
    (fromStringS "2") `shouldSatisfy` (_ `FFI.lessThan` (fromStringS "5"))
    (fromStringS "5") `shouldSatisfy` (_ `FFI.lessThanOrEqual` (fromStringS "5"))

    -- modulo, note the sign of the answers
    (FFI.modulo (fromStringS "5") (fromStringS "3")) `shouldEqual` (fromStringS "2")
    (FFI.modulo (fromStringS "-5") (fromStringS "3")) `shouldEqual` (fromStringS "-2")
    (FFI.modulo (fromStringS "5") (fromStringS "-3")) `shouldEqual` (fromStringS "2")
    (FFI.modulo (fromStringS "-5") (fromStringS "-3")) `shouldEqual` (fromStringS "-2")

    (FFI.multiply (fromStringS "5") (fromStringS "3")) `shouldEqual` (fromStringS "15")
    (FFI.negate (fromStringS "5")) `shouldEqual` (fromStringS "-5")
    (FFI.not (fromStringS "-12345")) `shouldEqual` (fromStringS "12344")
    (fromStringS "12344") `shouldSatisfy` (FFI.notEquals (fromStringS "-12345") )
    (FFI.or (fromStringS "11") (fromStringS "5")) `shouldEqual` (fromStringS "15")
    (FFI.shiftLeft (fromStringS "11") (fromStringS "2")) `shouldEqual` (fromStringS "44")
    (FFI.shiftRight (fromStringS "-11") (fromStringS "2")) `shouldEqual` (fromStringS "-3")
    (FFI.shiftRightUnsigned (fromStringU "18446744073709551605") (fromStringU "2")) `shouldEqual` (fromStringU "4611686018427387901")
    -- TODO rotateLeft, rotateRight
    (FFI.subtract (fromStringS "2") (fromStringS "3")) `shouldEqual` (fromStringS "-1")
    (FFI.toBytes sampleS.value isLittleEndianV) `shouldEqual` sampleS.leBytes
    (FFI.toBytes sampleS.value isBigEndianV) `shouldEqual` sampleS.beBytes
    (FFI.toInt (fromStringS "2")) `shouldEqual` 2
    -- out of range gets clipped
    (FFI.toInt (fromStringS "100000000000")) `shouldEqual` (Internal.numberBitsToInt 100000000000.0)

    -- can lose precision when converting to number
    (FFI.toNumber (fromStringS "9007199254740993")) `shouldEqual` 9007199254740992.0

    (FFI.toSigned (fromStringU "18446744073709551605")) `shouldEqual` (fromStringS "-11")
    (FFI.toUnsigned (fromStringS "-11")) `shouldEqual` (fromStringU "18446744073709551605")
    (FFI.xor (fromStringS "11") (fromStringS "5")) `shouldEqual` (fromStringS "14")

fromIntS :: Int -> Long
fromIntS i = runFn2 FFI.fromInt i isSignedV

fromIntU :: Int -> Long
fromIntU i = runFn2 FFI.fromInt i isUnsignedV

fromStringS :: String -> Long
fromStringS s = runFn3 FFI.fromString s isSignedV (Radix 10)

fromStringU :: String -> Long
fromStringU s = runFn3 FFI.fromString s isUnsignedV (Radix 10)

-- Constants

isSignedV :: IsUnsigned
isSignedV = IsUnsigned false

isUnsignedV :: IsUnsigned
isUnsignedV = IsUnsigned true

isBigEndianV :: IsLittleEndian
isBigEndianV = IsLittleEndian false

isLittleEndianV :: IsLittleEndian
isLittleEndianV = IsLittleEndian true

-- Sample

sampleS ::
  { value :: Long
  , high :: Int
  , low :: Int
  , beBytes :: Array Int
  , leBytes :: Array Int
  }
sampleS =
  { value: fromStringS "-107374182489"
  , high: -26
  , low: -89
  , beBytes
  , leBytes: Array.reverse beBytes
  }
  where
    beBytes = [255, 255, 255, 230, 255, 255, 255, 167]

sampleU ::
  { value :: Long
  , high :: Int
  , low :: Int
  }
sampleU =
  { value: fromStringU "18446743983515238366"
  , high: -22
  , low: -34
  }

module Data.Long.FFITest
       ( testFFI
       ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (runFn1, runFn2, runFn3)
import Data.Long.FFI (IsLittleEndian(..), IsUnsigned(..), Long, Radix(..))
import Data.Long.FFI as FFI
import Data.Long.Internal as Internal
import Effect (Effect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Test.Assert (assert, assertEqual, assertTrue)

testFFI :: Effect Unit
testFFI = do
  log "Constants"
  assertEq FFI.zero (fromIntS 0)
  assertEq FFI.one (fromIntS 1)
  assertEq FFI.negOne (fromIntS (-1))
  assertEq FFI.uzero (fromIntU 0)
  assertEq FFI.uone (fromIntU 1)
  assertEq FFI.maxValue (fromStringS "9223372036854775807")
  assertEq FFI.minValue (fromStringS "-9223372036854775808")
  assertEq FFI.maxUnsignedValue (fromStringU "18446744073709551615")

  log "Utilities"
  assert $ FFI.isLong (unsafeToForeign FFI.zero)
  assertEq (runFn3 FFI.fromBits sampleS.low sampleS.high isSignedV) sampleS.value
  assertEq (runFn3 FFI.fromBits sampleU.low sampleU.high isUnsignedV) sampleU.value
  assertEq (runFn3 FFI.fromBytes sampleS.beBytes isSignedV isBigEndianV) sampleS.value
  assertEq (runFn2 FFI.fromBytesLE sampleS.leBytes isSignedV) sampleS.value
  assertEq (runFn2 FFI.fromBytesBE sampleS.beBytes isSignedV) sampleS.value
  assertEq (runFn2 FFI.fromInt 2 isSignedV) (fromStringS "2")
  assertEq (runFn2 FFI.fromNumber 2.0 isSignedV) (fromStringS "2")

  log "Fields"
  assertEqual { actual: FFI.unsigned (fromStringU "2")
              , expected: isUnsignedV
              }

  log "Methods"
  assertEq (FFI.add (fromStringS "2") (fromStringS "3")) (fromStringS "5")
  assertEq (FFI.and (fromStringS "2") (fromStringS "1")) (fromStringS "0")
  assertEqual { actual: (FFI.compare (fromStringS "2") (fromStringS "1"))
              , expected: 1
              }
  assertEq (FFI.divide (fromStringS "8") (fromStringS "3")) (fromStringS "2")
  assertEqual { actual: Internal.numberBitsToInt $ FFI.getHighBits sampleS.value
              , expected: sampleS.high
              }
  assertEqual { actual: Internal.numberBitsToInt $ FFI.getHighBitsUnsigned sampleU.value
              , expected: sampleU.high
              }
  assertEqual { actual: Internal.numberBitsToInt $ FFI.getLowBits sampleS.value
              , expected: sampleS.low
              }
  assertEqual { actual: Internal.numberBitsToInt $ FFI.getLowBitsUnsigned sampleU.value
              , expected: sampleU.low
              }
  assertTrue $ FFI.greaterThan (fromStringS "5") (fromStringS "2")
  assertTrue $ FFI.greaterThanOrEqual (fromStringS "5") (fromStringS "5")
  assertTrue $ FFI.isEven (fromStringS "6")
  assertTrue $ FFI.isNegative (fromStringS "-6")
  assertTrue $ FFI.isOdd (fromStringS "5")
  assertTrue $ FFI.isPositive (fromStringS "5")
  assertTrue $ FFI.isZero FFI.zero
  assertTrue $ FFI.lessThan (fromStringS "2") (fromStringS "5")
  assertTrue $ FFI.lessThanOrEqual (fromStringS "5") (fromStringS "5")

  -- modulo, note the sign of the answers
  assertEq (FFI.modulo (fromStringS "5") (fromStringS "3")) (fromStringS "2")
  assertEq (FFI.modulo (fromStringS "-5") (fromStringS "3")) (fromStringS "-2")
  assertEq (FFI.modulo (fromStringS "5") (fromStringS "-3")) (fromStringS "2")
  assertEq (FFI.modulo (fromStringS "-5") (fromStringS "-3")) (fromStringS "-2")

  assertEq (FFI.multiply (fromStringS "5") (fromStringS "3")) (fromStringS "15")
  assertEq (FFI.negate (fromStringS "5")) (fromStringS "-5")
  assertEq (FFI.not (fromStringS "-12345")) (fromStringS "12344")
  assertTrue (FFI.notEquals (fromStringS "-12345") (fromStringS "12344"))
  assertEq (FFI.or (fromStringS "11") (fromStringS "5")) (fromStringS "15")
  assertEq (FFI.shiftLeft (fromStringS "11") (fromStringS "2")) (fromStringS "44")
  assertEq (FFI.shiftRight (fromStringS "-11") (fromStringS "2")) (fromStringS "-3")
  assertEq (FFI.shiftRightUnsigned (fromStringU "18446744073709551605") (fromStringU "2")) (fromStringU "4611686018427387901")
  -- TODO rotateLeft, rotateRight
  assertEq (FFI.subtract (fromStringS "2") (fromStringS "3")) (fromStringS "-1")
  assertEqual { actual: FFI.toBytes sampleS.value isLittleEndianV
              , expected: sampleS.leBytes
              }
  assertEqual { actual: FFI.toBytes sampleS.value isBigEndianV
              , expected: sampleS.beBytes
              }
  assertEqual { actual: FFI.toInt (fromStringS "2")
              , expected: 2
              }
  -- out of range gets clipped
  assertEqual { actual: FFI.toInt (fromStringS "100000000000")
              , expected: Internal.numberBitsToInt 100000000000.0
              }

  -- can lose precision when converting to number
  assertEqual { actual: FFI.toNumber (fromStringS "9007199254740993")
              , expected: 9007199254740992.0
              }

  assertEq (FFI.toSigned (fromStringU "18446744073709551605")) (fromStringS "-11")
  assertEq (FFI.toUnsigned (fromStringS "-11")) (fromStringU "18446744073709551605")
  assertEq (FFI.xor (fromStringS "11") (fromStringS "5")) (fromStringS "14")

fromIntS :: Int -> Long
fromIntS i = runFn2 FFI.fromInt i isSignedV

fromIntU :: Int -> Long
fromIntU i = runFn2 FFI.fromInt i isUnsignedV

fromStringS :: String -> Long
fromStringS s = runFn3 FFI.fromString s isSignedV (Radix 10)

fromStringU :: String -> Long
fromStringU s = runFn3 FFI.fromString s isUnsignedV (Radix 10)

showLong :: Long -> String
showLong l = runFn1 FFI.toString l (Radix 10)

assertEq :: Long -> Long -> Effect Unit
assertEq actual expected = do
  assertEqual { actual: FFI.unsigned actual
              , expected: FFI.unsigned expected
              }
  -- Additional string based equal check to aid in
  -- debugging
  assertEqual { actual: showLong actual
              , expected: showLong expected
              }
  assert (FFI.equals actual expected)

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

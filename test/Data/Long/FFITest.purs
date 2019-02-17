module Data.Long.FFITest
       ( testFFI
       ) where

import Prelude

import Data.Function.Uncurried (runFn2, runFn3)
import Data.Long.FFI (IsUnsigned(..), Long, Radix(..))
import Data.Long.FFI as FFI
import Effect (Effect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Test.Assert (assert, assertEqual)

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


fromIntS :: Int -> Long
fromIntS i = runFn2 FFI.fromInt i (IsUnsigned false)

fromIntU :: Int -> Long
fromIntU i = runFn2 FFI.fromInt i (IsUnsigned true)

fromStringS :: String -> Long
fromStringS s = runFn3 FFI.fromString s (IsUnsigned false) (Radix 10)

fromStringU :: String -> Long
fromStringU s = runFn3 FFI.fromString s (IsUnsigned true) (Radix 10)

assertEq :: Long -> Long -> Effect Unit
assertEq actual expected = do
  assertEqual { actual: FFI.unsigned actual
              , expected: FFI.unsigned expected
              }
  assert (FFI.equals actual expected)

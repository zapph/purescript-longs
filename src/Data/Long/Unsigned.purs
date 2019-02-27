module Data.Long.Unsigned
       ( Long
       , fromLowHighBits
       , fromInt
       , fromNumber
       , fromString
       , fromStringAs
       , lowBits
       , highBits
       , toInt
       , toNumber
       , toString
       , toStringAs
       , parity
       , even
       , odd
       , quot
       , rem
       , toSigned
       ) where

import Data.Int (Parity, Radix)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe)

type Long = Internal.Long Internal.Unsigned

-- | Creates a `Long` from an `Int` value
fromInt :: Int -> Maybe Long
fromInt = Internal.unsignedLongFromInt

-- | Creates an `Long` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Long` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe Long
fromNumber = Internal.fromNumber

-- | Creates a signed `Long` from low and high bits respresented as `Int`
fromLowHighBits :: Int -> Int -> Long
fromLowHighBits = Internal.fromLowHighBits

-- | Reads an `Long` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Long` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe Long
fromString = Internal.fromString

-- | Like `fromString`, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe Long
fromStringAs = Internal.fromStringAs

-- | Get low bits of a `Long` as an `Int`
lowBits :: Long -> Int
lowBits = Internal.lowBits

-- | Get high bits of a `Long` as an `Int`
highBits :: Long -> Int
highBits = Internal.highBits

-- | Creates an `Int` if the `Long` value is within the range of `Long`.
toInt :: Long -> Maybe Int
toInt = Internal.toInt

-- | Creates a `Number` value from a `Long`. Values greater than
-- | `Number.MAX_SAFE_INTEGER` will lose precision.
toNumber :: Long -> Number
toNumber = Internal.toNumber

-- | Like `show`, but omits the `ul` suffix.
toString :: Long -> String
toString = Internal.toString

-- | Like `toStringAs`, but the integer can be specified in a different base.
toStringAs :: Radix -> Long -> String
toStringAs = Internal.toStringAs

-- | Returns whether a `Long` is `Even` or `Odd`.
parity :: Long -> Parity
parity = Internal.parity

-- | Returns whether a `Long` is an even number.
even :: Long -> Boolean
even = Internal.even

-- | Returns whether a `Long` is an odd number.
odd :: Long -> Boolean
odd = Internal.odd

-- | For unsigned longs, `quot` is identical to `div`.
quot :: Long -> Long -> Long
quot = Internal.quot

-- | For unsigned longs, `quot` is identical to `mod`.
rem :: Long -> Long -> Long
rem = Internal.rem

-- | Converts to a signed long by reading the bits as a 2's complement 64 bit signed integer.
toSigned :: Long -> Internal.Long Internal.Signed
toSigned = Internal.unsignedToSigned

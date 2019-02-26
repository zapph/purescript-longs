module Data.Long
       ( Long
       , fromLowHighBits
       , fromInt
       , fromString
       , fromStringAs
       , lowBits
       , highBits
       , toInt
       , toString
       , toStringAs
       , parity
       , even
       , odd
       , quot
       , rem
       , toUnsigned
       ) where

import Data.Int (Parity, Radix)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe)

type Long = Internal.Long Internal.Signed

--| Creates a `Long` from an `Int` value
fromInt :: Int -> Long
fromInt = Internal.signedLongFromInt

--| Creates a signed `Long` from low and high bits respresented as `Int`
fromLowHighBits :: Int -> Int -> Long
fromLowHighBits = Internal.fromLowHighBits

--| Reads an Int from a String value. The number must parse as an integer and fall within the valid range of values for the Int type, otherwise Nothing is returned.
fromString :: String -> Maybe Long
fromString = Internal.fromString

--| Like fromString, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe Long
fromStringAs = Internal.fromStringAs

--| Get low bits of a `Long` as an `Int`
lowBits :: Long -> Int
lowBits = Internal.lowBits

--| Get high bits of a `Long` as an `Int`
highBits :: Long -> Int
highBits = Internal.highBits

--| Creates an `Int` if the `Long` value is within the range of `Long`.
toInt :: Long -> Maybe Int
toInt = Internal.toInt

--| Like `show`, but omits the `l` suffix.
toString :: Long -> String
toString = Internal.toString

--| Like `toStringAs`, but the integer can be specified in a different base.
toStringAs :: Radix -> Long -> String
toStringAs = Internal.toStringAs

--| Converts a `Long` to a `Number`, possibly losing precision.
toNumber :: Long -> Number
toNumber = Internal.toNumber

--| Returns whether a `Long` is `Even` or `Odd`.
parity :: Long -> Parity
parity = Internal.parity

--| Returns whether a `Long` is an even number.
even :: Long -> Boolean
even = Internal.even

--| Returns whether a `Long` is an odd number.
odd :: Long -> Boolean
odd = Internal.odd

--| The quot function provides truncating integer division (see the documentation for the EuclideanRing class). It is identical to div in the EuclideanRing Long instance if the dividend is positive, but will be slightly different if the dividend is negative.
quot :: Long -> Long -> Long
quot = Internal.quot

--| The rem function provides the remainder after truncating integer division (see the documentation for the EuclideanRing class). It is identical to mod in the EuclideanRing Long instance if the dividend is positive, but will be slightly different if the dividend is negative
rem :: Long -> Long -> Long
rem = Internal.rem

--| Converts to an unsigned long by reading the bits as a 64 bit unsigned integer.
toUnsigned :: Long -> Internal.Long Internal.Unsigned
toUnsigned = Internal.signedToUnsigned

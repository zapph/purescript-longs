module Data.Long
       ( Long
       , fromLowHigh
       , fromInt
       , fromString
       , fromStringAs
       , toInt
       , toString
       , toStringAs
       , parity
       , even
       , odd
       ) where

import Data.Int (Parity, Radix)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe)

type Long = Internal.Long Internal.Signed

--| Creates a `Long` from an `Int` value
fromInt :: Int -> Long
fromInt = Internal.fromInt

--| Creates a `Long` from low and high bytes respresented as `Int`
fromLowHigh :: Int -> Int -> Long
fromLowHigh = Internal.fromLowHigh

--| Reads an Int from a String value. The number must parse as an integer and fall within the valid range of values for the Int type, otherwise Nothing is returned.
fromString :: String -> Maybe Long
fromString = Internal.fromString

--| Like fromString, but the integer can be specified in a different base.
fromStringAs :: Radix -> String -> Maybe Long
fromStringAs = Internal.fromStringAs

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

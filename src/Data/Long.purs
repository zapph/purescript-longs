module Data.Long
       ( Long
       , fromLowHigh
       , fromInt
       , fromString
       , toInt
       , toString
       ) where

import Data.Int (Radix)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe)

type Long = Internal.Long Internal.Signed

--| Creates a `Long` from an `Int` value
fromInt :: Int -> Long
fromInt = Internal.fromInt

--| Creates a `Long` from low and high bytes respresented as `Int`
fromLowHigh :: Int -> Int -> Long
fromLowHigh = Internal.fromLowHigh

fromString :: String -> Radix -> Maybe Long
fromString = Internal.fromString

--| Creates an `Int` if the `Long` value is within the range of `Long`.
toInt :: Long -> Maybe Int
toInt = Internal.toInt

toString :: Long -> Radix -> String
toString = Internal.toString

--| Converts a `Long` to a `Number`, possibly losing precision.
toNumber :: Long -> Number
toNumber = Internal.toNumber

module Data.Long
       ( Long
       , fromLowHigh
       , fromInt
       , fromString
       , toInt
       , toString
       ) where

import Prelude

import Data.Function.Uncurried (runFn2, runFn3)
import Data.Int (Radix, decimal)
import Data.Int as Int
import Data.Long.FFI (IsUnsigned(..))
import Data.Long.FFI as FFI
import Data.Long.Internal as Internal
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Long = Long FFI.Long

instance showLong :: Show Long where
  show (Long l) = show l

instance eqLong :: Eq Long where
  eq (Long l1) (Long l2) = FFI.equals l1 l2

instance ordLong :: Ord Long where
  compare (Long l1) (Long l2) = case FFI.compare l1 l2 of
    0 -> EQ
    x | x > 0 -> GT
    _ -> LT

instance boundedLong :: Bounded Long where
  bottom = Long $ FFI.minValue
  top = Long $ FFI.maxValue

instance semiringLong :: Semiring Long where
  add (Long l1) (Long l2) = Long $ FFI.add l1 l2
  zero = Long $ FFI.zero
  mul (Long l1) (Long l2) = Long $ FFI.multiply l1 l2
  one = Long $ FFI.one

instance ringLong :: Ring Long where
  sub (Long l1) (Long l2) = Long $ FFI.subtract l1 l2

instance commutativeRingLong :: CommutativeRing Long

instance euclideanRingLong :: EuclideanRing Long where
  degree = Int.floor <<< toNumber <<< abs
  div (Long l1) (Long l2) = Long $ FFI.divide l1 l2
  mod (Long l1) l2l@(Long l2) = Long $ FFI.modulo l1 l2

instance arbitraryLong :: Arbitrary Long where
  arbitrary = fromLowHigh <$> arbitrary <*> arbitrary

--| Creates a `Long` from an `Int` value
fromInt :: Int -> Long
fromInt i = Long $ runFn2 FFI.fromInt i isSignedV

--| Creates a `Long` from low and high bytes respresented as `Int`
fromLowHigh :: Int -> Int -> Long
fromLowHigh l h = Long $ runFn3 FFI.fromBits l h isSignedV

fromString :: String -> Radix -> Maybe Long
fromString s radix = Long <$> Internal.safeReadLong s isSignedV radix

--| Creates an `Int` if the `Long` value is within the range of `Long`.
toInt :: Long -> Maybe Int
toInt l'@(Long l) | l' >= intMinValueL && l' <= intMaxValueL = Just $ FFI.toInt l
toInt _ = Nothing

toString :: Long -> String
toString (Long l) = FFI.toString l decimal

--| Converts a `Long` to a `Number`, possibly losing precision.
toNumber :: Long -> Number
toNumber (Long l) = FFI.toNumber l

-- constants
isSignedV :: IsUnsigned
isSignedV = IsUnsigned false

intMaxValueL :: Long
intMaxValueL = fromInt top

intMinValueL :: Long
intMinValueL = fromInt bottom

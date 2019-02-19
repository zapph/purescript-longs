module Data.Long
       ( Long
       , fromLowHigh
       , fromInt
       , fromString
       , toInt
       , toString
       ) where

import Prelude

import Data.Foldable (find)
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Long.FFI (IsUnsigned(..))
import Data.Long.FFI as FFI
import Data.Maybe (Maybe(..))
import Effect.Exception (catchException)
import Effect.Uncurried (runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
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

instance arbitraryLong :: Arbitrary Long where
  arbitrary = fromLowHigh <$> arbitrary <*> arbitrary

--| Creates a `Long` from an `Int` value
fromInt :: Int -> Long
fromInt i = Long $ runFn2 FFI.fromInt i isSignedV

--| Creates a `Long` from low and high bytes respresented as `Int`
fromLowHigh :: Int -> Int -> Long
fromLowHigh l h = Long $ runFn3 FFI.fromBits l h isSignedV

fromString :: String -> Maybe Long
fromString "-0" = Just $ Long FFI.zero -- change to prelude zero
fromString s =
  -- converting back to string is a lousy way of doing this, but
  -- long.js does not guard against out of bounds. should find a better
  -- way
  -- Relevant: https://github.com/dcodeIO/long.js/issues/42
  Long <$> find (isSameWithInput) l'
  where
    l' =
      unsafePerformEffect
      $ catchException (\_ -> pure Nothing)
      $ Just <$> runEffectFn3 FFI.fromString s isSignedV radix10

    isSameWithInput l = s == FFI.toString l radix10


-- todo just
--| Creates an `Int` if the `Long` value is within the range of `Long`.
toInt :: Long -> Maybe Int
toInt l'@(Long l) | l' >= intMinValueL && l' <= intMaxValueL = Just $ FFI.toInt l
toInt _ = Nothing


toString :: Long -> String
toString (Long l) = FFI.toString l radix10

-- constants
isSignedV :: IsUnsigned
isSignedV = IsUnsigned false

intMaxValueL :: Long
intMaxValueL = fromInt top

intMinValueL :: Long
intMinValueL = fromInt bottom

radix10 :: FFI.Radix
radix10 = FFI.Radix 10

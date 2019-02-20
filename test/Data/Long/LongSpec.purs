module Data.LongSpec
       ( longSpec
       ) where

import Prelude

import Data.Int (decimal)
import Data.Long (Long)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Data (checkCommutativeRing, checkEq, checkEuclideanRing, checkOrd, checkRing, checkSemiring)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

-- Helper for Longs within the Int range
newtype IntInLong = IntInLong Long
instance arbitraryIntInLong :: Arbitrary IntInLong where
  arbitrary = IntInLong <<< Long.fromInt <$> arbitrary

derive newtype instance eqIntInLong :: Eq IntInLong
derive newtype instance semiringIntInLong :: Semiring IntInLong
derive newtype instance ringIntInLong :: Ring IntInLong
derive newtype instance commutativeRingIntInLong :: CommutativeRing IntInLong
derive newtype instance eucledianRingIntInLong :: EuclideanRing IntInLong

longSpec :: Spec Unit
longSpec = describe "Long" do
  it "should follow laws" $ liftEffect do
    checkEq prxLong
    checkOrd prxLong
    checkSemiring prxLong
    checkRing prxLong
    checkCommutativeRing prxLong
    -- since degree is only up to int, we can only check for IntInLong
    checkEuclideanRing prxIntInLong

  it "should convert ints" $
    quickCheck \i -> Long.toInt (Long.fromInt i) == Just i

  it "should return Nothing for toInt when out of bounds" do
    Long.toInt (fromString "2147483648") `shouldSatisfy` isNothing
    Long.toInt (fromString "-2147483649") `shouldSatisfy` isNothing

  it "should convert to strings" $
    quickCheck \l -> Long.fromString (Long.toString l) decimal == Just l

  it "should have an ord instance following ints within range" do
    quickCheck \i j -> (compare i j) == compare (Long.fromInt i) (Long.fromInt j)


fromString :: String -> Long
fromString s = unsafePartial $ fromJust $ Long.fromString s decimal

prxLong :: Proxy Long
prxLong = Proxy

prxIntInLong :: Proxy IntInLong
prxIntInLong = Proxy
module Data.LongSpec
       ( longSpec
       ) where

import Prelude

import Data.Long (Long)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

longSpec :: Spec Unit
longSpec = describe "Long" do
  it "should convert ints" $
    quickCheck \i -> Long.toInt (Long.fromInt i) == Just i

  it "should return Nothing for toInt when out of bounds" do
    Long.toInt (fromString "2147483648") `shouldSatisfy` isNothing
    Long.toInt (fromString "-2147483649") `shouldSatisfy` isNothing

  it "should convert to strings" $
    quickCheck \l -> Long.fromString (Long.toString l) == Just l

  it "should return Nothing for unknown strings" do
    Long.fromString "asdf" `shouldSatisfy` isNothing
    Long.fromString " 234" `shouldSatisfy` isNothing
    Long.fromString "2 34" `shouldSatisfy` isNothing
    Long.fromString "2-34" `shouldSatisfy` isNothing

  it "should return Nothing for out of bound strings" do
    Long.fromString "9223372036854775808" `shouldSatisfy` isNothing
    Long.fromString "-9223372036854775809" `shouldSatisfy` isNothing

  it "should read 0 strings" $ do
    Long.fromString "0" `shouldSatisfy` isJust
    Long.fromString "-0" `shouldSatisfy` isJust

  it "should have an ord instance following ints within range" do
    quickCheck \i j -> (compare i j) == compare (Long.fromInt i) (Long.fromInt j)


fromString :: String -> Long
fromString s = unsafePartial $ fromJust $ Long.fromString s

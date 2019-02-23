module Data.Long.InternalSpec
       ( internalSpec
       ) where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Int (Radix, binary, decimal, hexadecimal, octal, radix)
import Data.Long.Internal (class SInfo, Long, SignProxy(..), Signed, Unsigned)
import Data.Long.Internal as Internal
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global as Number
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Data (checkCommutativeRing, checkEq, checkEuclideanRing, checkOrd, checkRing, checkSemiring)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

internalSpec :: Spec Unit
internalSpec = do
  longSpec
  fromStringSpec

longSpec :: Spec Unit
longSpec = describe "Long" do
  it "should follow laws" $ liftEffect do
    checkEq prxSignedLong
    checkOrd prxSignedLong
    checkSemiring prxSignedLong
    checkRing prxSignedLong
    checkCommutativeRing prxSignedLong
    -- since degree is only up to int, we can only check for IntInLong
    checkEuclideanRing prxIntInSignedLong

    checkEq prxUnsignedLong
    checkOrd prxUnsignedLong
    checkSemiring prxUnsignedLong
    checkRing prxUnsignedLong
    checkCommutativeRing prxUnsignedLong

  it "should convert ints" $
    quickCheck \i -> Internal.toInt (Internal.fromInt i :: Long Signed) == Just i

  it "should convert to strings" $ do
    quickCheck \(Radix' r) l ->
      readSigned r (Internal.toStringAs r l) == Just l

    quickCheck \(Radix' r) l ->
      readUnsigned r (Internal.toStringAs r l) == Just l

  it "should convert numbers" $ do
    traverse_ (checkNumber signedProxy)
      [ 0.0
      , 9223372036854775807.0
      , (-9223372036854775808.0)
      , 2e10
      ]

    traverse_ (checkNumber unsignedProxy)
      [ 0.0
      , 9223372036854775807.0
      , (18446744073709551615.0)
      , 2e10
      ]

  it "should reject conversion from non whole numbers" $ do
    traverse_ (\n -> Internal.fromNumber n :: Maybe (Long Signed) `shouldSatisfy` isNothing)
      [ 5.5
      , 100.1
      , 200.25
      , 0.000001
      -- , 999999999.00000001 -- Number is not precise enough to store the decimal part
      , Number.nan
      , Number.infinity
      ]

  it "should reject conversion of numbers outside the long range" $ do
    traverse_ (\n -> Internal.fromNumber n :: Maybe (Long Signed) `shouldSatisfy` isNothing)
      [ -10000000000000000000.0 -- Must be big enough to store precision
      , 10000000000000000000.0
      ]

    traverse_ (\n -> Internal.fromNumber n :: Maybe (Long Unsigned) `shouldSatisfy` isNothing)
      [ -1.0
      , 20000000000000000000.0
      ]


checkNumber :: forall s. (SInfo s) => SignProxy s -> Number -> Aff Unit
checkNumber _ n =
  (Internal.toNumber <$> (Internal.fromNumber n :: Maybe (Long s))) `shouldEqual` Just n

fromStringSpec :: Spec Unit
fromStringSpec = describe "fromString" do
  it "should leave valid strings unchanged" do
    readSigned decimal "12345" `shouldEqual` Just (i2lS 12345)
    readSigned decimal "+12345" `shouldEqual` Just (i2lS 12345)
    readSigned decimal "-12345" `shouldEqual` Just (i2lS (-12345))
    readSigned decimal "-12345" `shouldEqual` Just (i2lS (-12345))

  it "should read signed zeros" do
    readSigned decimal "0" `shouldEqual` Just zero
    readSigned decimal "-0" `shouldEqual` Just zero
    readSigned decimal "000" `shouldEqual` Just zero
    readSigned decimal "-00" `shouldEqual` Just zero

  it "should read unsigned zeros" do
    readUnsigned decimal "0" `shouldEqual` Just zero
    readUnsigned decimal "-0" `shouldEqual` Just zero
    readUnsigned decimal "000" `shouldEqual` Just zero
    readUnsigned decimal "-00" `shouldEqual` Just zero

  it "should return Nothing on empty string" do
    readSigned decimal "" `shouldEqual` Nothing

  it "should disallow negative for unsigned" do
    readUnsigned decimal "-123" `shouldEqual` Nothing

  it "should disallow invalid characters depending on radix" do
    readSigned binary "1010" `shouldSatisfy` isJust
    readSigned binary "-1010" `shouldSatisfy` isJust
    readSigned binary "1012" `shouldSatisfy` isNothing

    readSigned octal "1234" `shouldSatisfy` isJust
    readSigned octal "1834" `shouldSatisfy` isNothing

    readSigned hexadecimal "1bcd" `shouldSatisfy` isJust
    readSigned hexadecimal "1BCd" `shouldSatisfy` isJust
    readSigned hexadecimal "1bcz" `shouldSatisfy` isNothing

  it "should read at the limits" do
    readSigned decimal "9223372036854775807" `shouldEqual` Just top
    readSigned decimal "+009223372036854775807" `shouldEqual` Just top
    readSigned decimal "-09223372036854775808" `shouldEqual` Just bottom

    readUnsigned decimal "18446744073709551615" `shouldEqual` Just top

    readSigned hexadecimal "7fffffffffffffff" `shouldEqual` Just top
    readSigned hexadecimal "-8000000000000000" `shouldEqual` Just bottom

  it "should fail for overflows" do
    readSigned decimal "9223372036854775808" `shouldSatisfy` isNothing
    readSigned decimal "-9223372036854775809" `shouldSatisfy` isNothing

    readUnsigned decimal "18446744073709551616" `shouldSatisfy` isNothing

    readSigned hexadecimal "8000000000000000" `shouldSatisfy` isNothing
    readSigned hexadecimal "-8000000000000001" `shouldSatisfy` isNothing

readSigned :: Radix -> String -> Maybe (Long Signed)
readSigned = Internal.fromStringAs

readUnsigned :: Radix -> String -> Maybe (Long Unsigned)
readUnsigned = Internal.fromStringAs

i2lS :: Int -> Long Signed
i2lS = Internal.fromInt

i2lU :: Int -> Long Signed
i2lU = Internal.fromInt

prxSignedLong :: Proxy (Long Signed)
prxSignedLong = Proxy

prxUnsignedLong :: Proxy (Long Unsigned)
prxUnsignedLong = Proxy

prxIntInSignedLong :: Proxy IntInSignedLong
prxIntInSignedLong = Proxy

signedProxy :: SignProxy Signed
signedProxy = SignProxy

unsignedProxy :: SignProxy Unsigned
unsignedProxy = SignProxy

-- Helper for Longs within the Int range
newtype IntInSignedLong = IntInSignedLong (Long Signed)
instance arbitraryIntInSignedLong :: Arbitrary IntInSignedLong where
  arbitrary = IntInSignedLong <<< Internal.fromInt <$> arbitrary

derive newtype instance eqIntInSignedLong :: Eq IntInSignedLong
derive newtype instance semiringIntInSignedLong :: Semiring IntInSignedLong
derive newtype instance ringIntInSignedLong :: Ring IntInSignedLong
derive newtype instance commutativeRingIntInSignedLong :: CommutativeRing IntInSignedLong
derive newtype instance eucledianRingIntInSignedLong :: EuclideanRing IntInSignedLong

newtype Radix' = Radix' Radix
instance arbitraryRadix' :: Arbitrary Radix' where
  arbitrary = chooseInt 2 36 >>= \i ->
    case radix i of
      Just r -> pure (Radix' r)
      Nothing -> arbitrary

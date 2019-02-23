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
    quickCheck \l (Radix' r) ->
      readSigned (Internal.toString l r) r == Just l

    quickCheck \l (Radix' r) ->
      readUnsigned (Internal.toString l r) r == Just l

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
    readSigned "12345" decimal `shouldEqual` Just (i2lS 12345)
    readSigned "+12345" decimal `shouldEqual` Just (i2lS 12345)
    readSigned "-12345" decimal `shouldEqual` Just (i2lS (-12345))
    readSigned "-12345" decimal `shouldEqual` Just (i2lS (-12345))

  it "should read signed zeros" do
    readSigned "0" decimal `shouldEqual` Just zero
    readSigned "-0" decimal `shouldEqual` Just zero
    readSigned "000" decimal `shouldEqual` Just zero
    readSigned "-00" decimal `shouldEqual` Just zero

  it "should read unsigned zeros" do
    readUnsigned "0" decimal `shouldEqual` Just zero
    readUnsigned "-0" decimal `shouldEqual` Just zero
    readUnsigned "000" decimal `shouldEqual` Just zero
    readUnsigned "-00" decimal `shouldEqual` Just zero

  it "should return Nothing on empty string" do
    readSigned "" decimal `shouldEqual` Nothing

  it "should disallow negative for unsigned" do
    readUnsigned "-123" decimal `shouldEqual` Nothing

  it "should disallow invalid characters depending on radix" do
    readSigned "1010" binary `shouldSatisfy` isJust
    readSigned "-1010" binary `shouldSatisfy` isJust
    readSigned "1012" binary `shouldSatisfy` isNothing

    readSigned "1234" octal `shouldSatisfy` isJust
    readSigned "1834" octal `shouldSatisfy` isNothing

    readSigned "1bcd" hexadecimal `shouldSatisfy` isJust
    readSigned "1BCd" hexadecimal `shouldSatisfy` isJust
    readSigned "1bcz" hexadecimal `shouldSatisfy` isNothing

  it "should read at the limits" do
    readSigned "9223372036854775807" decimal `shouldEqual` Just top
    readSigned "+009223372036854775807" decimal `shouldEqual` Just top
    readSigned "-09223372036854775808" decimal `shouldEqual` Just bottom

    readUnsigned "18446744073709551615" decimal `shouldEqual` Just top

    readSigned "7fffffffffffffff" hexadecimal `shouldEqual` Just top
    readSigned "-8000000000000000" hexadecimal `shouldEqual` Just bottom

  it "should fail for overflows" do
    readSigned "9223372036854775808" decimal `shouldSatisfy` isNothing
    readSigned "-9223372036854775809" decimal `shouldSatisfy` isNothing

    readUnsigned "18446744073709551616" decimal `shouldSatisfy` isNothing

    readSigned "8000000000000000" hexadecimal `shouldSatisfy` isNothing
    readSigned "-8000000000000001" hexadecimal `shouldSatisfy` isNothing

readSigned :: String -> Radix -> Maybe (Long Signed)
readSigned = Internal.fromString

readUnsigned :: String -> Radix -> Maybe (Long Unsigned)
readUnsigned = Internal.fromString

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

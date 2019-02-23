module Data.Long.Internal
       ( Long
       , kind Signedness
       , Signed
       , Unsigned
       , class SInfo
       , ffiSignedness
       , ffiTop
       , ffiBottom
       , ffiZero
       , ffiOne
       , SignProxy(..)
       , fromLowHigh
       , fromInt
       , fromNumber
       , fromString
       , fromStringAs
       , toInt
       , toString
       , toStringAs
       , toNumber
         -- Utils
       , numberBitsToInt
       ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn2, runFn3)
import Data.Int (Radix, decimal)
import Data.Int as Int
import Data.Long.FFI as FFI
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Ord (abs)
import Test.QuickCheck (class Arbitrary, arbitrary)

foreign import kind Signedness

foreign import data Signed :: Signedness
foreign import data Unsigned :: Signedness

data SignProxy (s :: Signedness) = SignProxy

class SInfo (s :: Signedness) where
  ffiSignedness :: SignProxy s -> FFI.IsUnsigned
  ffiTop :: SignProxy s -> FFI.Long
  ffiBottom :: SignProxy s -> FFI.Long
  ffiZero :: SignProxy s -> FFI.Long
  ffiOne :: SignProxy s -> FFI.Long

instance infoSigned :: SInfo Signed where
  ffiSignedness _ = (FFI.IsUnsigned false)
  ffiTop _ = FFI.maxValue
  ffiBottom _ = FFI.minValue
  ffiZero _ = FFI.zero
  ffiOne _ = FFI.one

instance infoUnsigned :: SInfo Unsigned where
  ffiSignedness _ = (FFI.IsUnsigned true)
  ffiTop _ = FFI.maxUnsignedValue
  ffiBottom _ = FFI.uzero
  ffiZero _ = FFI.uzero
  ffiOne _ = FFI.uone

newtype Long (s :: Signedness) = Long FFI.Long

instance showLong :: Show (Long s) where
  show (Long l) = show l

instance eqLong :: Eq (Long s) where
  eq (Long l1) (Long l2) = FFI.equals l1 l2

instance ordLong :: Ord (Long s) where
  compare (Long l1) (Long l2) = case FFI.compare l1 l2 of
    0 -> EQ
    x | x > 0 -> GT
    _ -> LT

instance boundedLong :: SInfo s => Bounded (Long s) where
  top = Long $ ffiTop (SignProxy :: SignProxy s)
  bottom = Long $ ffiBottom (SignProxy :: SignProxy s)

instance semiringLong :: SInfo s => Semiring (Long s) where
  add (Long l1) (Long l2) = Long $ FFI.add l1 l2
  zero = Long $ ffiZero (SignProxy :: SignProxy s)
  mul (Long l1) (Long l2) = Long $ FFI.multiply l1 l2
  one = Long $ ffiOne (SignProxy :: SignProxy s)

instance ringLong :: SInfo s => Ring (Long s) where
  sub (Long l1) (Long l2) = Long $ FFI.subtract l1 l2

instance commutativeRingLong :: SInfo s => CommutativeRing (Long s)

instance euclideanRingLong :: SInfo s => EuclideanRing (Long s) where
  degree = Int.floor <<< toNumber <<< abs
  div (Long l1) (Long l2) = Long $ FFI.divide l1 l2
  mod (Long l1) l2l@(Long l2) = Long $ FFI.modulo l1 l2

instance arbitraryLong :: SInfo s => Arbitrary (Long s) where
  arbitrary = fromLowHigh <$> arbitrary <*> arbitrary

-- Constructors

fromInt :: forall s. SInfo s => Int -> Long s
fromInt i = Long $ runFn2 FFI.fromInt i (ffiSignedness (SignProxy :: SignProxy s))

fromLowHigh :: forall s. SInfo s => Int -> Int -> Long s
fromLowHigh l h = Long $ runFn3 FFI.fromBits l h (ffiSignedness (SignProxy :: SignProxy s))

fromNumber :: forall s. SInfo s => Number -> Maybe (Long s)
fromNumber n =
  if isValidNumber
  then Just $ Long $ runFn2 FFI.fromNumber n (ffiSignedness p)
  else Nothing

  where
    isValidNumber = isWholeNumber n && isNumberInLongRange p n
    p = SignProxy :: SignProxy s

fromString :: forall s. SInfo s => String -> Maybe (Long s)
fromString = fromStringAs decimal

fromStringAs :: forall s. SInfo s => Radix -> String -> Maybe (Long s)
fromStringAs radix s =
  Long <$> safeReadLong s (ffiSignedness (SignProxy :: SignProxy s)) radix

toInt :: forall s. SInfo s => Long s -> Maybe Int
toInt l'@(Long l) | l' >= intBottomValueL && l' <= intTopValueL = Just $ FFI.toInt l
toInt _ = Nothing

toString :: forall s. Long s -> String
toString = toStringAs decimal

toStringAs :: forall s. Radix -> Long s -> String
toStringAs r (Long l) = FFI.toString l r

--| Converts a `Long` to a `Number`, possibly losing precision.
toNumber :: forall s. Long s -> Number
toNumber (Long l) = FFI.toNumber l

-- Utils

intTopValueL :: forall s. SInfo s => Long s
intTopValueL = fromInt top

intBottomValueL :: forall s. SInfo s => Long s
intBottomValueL = fromInt bottom

longTopValueN :: forall s. SInfo s => SignProxy s -> Number
longTopValueN _ = toNumber (top :: Long s)

longBottomValueN :: forall s. SInfo s => SignProxy s -> Number
longBottomValueN _ = toNumber (bottom :: Long s)

isNumberInLongRange :: forall s. SInfo s => SignProxy s -> Number -> Boolean
isNumberInLongRange p n = longBottomValueN p <= n && n <= longTopValueN p

foreign import numberBitsToInt :: Number -> Int

safeReadLong :: String -> FFI.IsUnsigned -> Radix -> Maybe FFI.Long
safeReadLong s isSigned radix =
  Nullable.toMaybe $ runFn3 _safeReadLong s isSigned radix

foreign import _safeReadLong :: Fn3 String FFI.IsUnsigned Radix (Nullable FFI.Long)

foreign import isWholeNumber :: Number -> Boolean

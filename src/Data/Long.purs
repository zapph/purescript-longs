module Data.Long
       ( Long
       , fromBits
       ) where

-- A 64 bit two's-complement integer
import Data.Function.Uncurried (Fn2, runFn2)

-- A 64 bit two's-complement integer
foreign import data Long :: Type

-- Constants
foreign import _zero :: Long
foreign import _one :: Long
foreign import _negOne :: Long
foreign import _maxValue :: Long
foreign import _minValue :: Long

-- Utilities
fromBits :: Int -> Int -> Long
fromBits = runFn2 _fromBits

foreign import _fromBits :: Fn2 Int Int Long

-- Returns a Long representing the given 32 bit integer value.
foreign import fromInt :: Int -> Long

-- Returns a Long representing the given value, provided that it is a finite number. Otherwise, zero is returned.
foreign import fromNumber :: Number -> Long

foreign import fromString :: String -> Boolean -> Int -> Long

foreign import _add :: Long -> Long -> Long
foreign import _and :: Long -> Long -> Long
foreign import _compare :: Long -> Long -> Long
foreign import _divide :: Long -> Long -> Long
foreign import _equals :: Long -> Long -> Long
foreign import _getHighBits :: Long -> Int
foreign import _getLowBits :: Long -> Int
foreign import _getNumBitsAbs :: Long -> Int
foreign import _greaterThan :: Long -> Long -> Long
foreign import _greaterThanOrEqual :: Long -> Long -> Long
foreign import _isEven :: Long -> Boolean
foreign import _isNegative :: Long -> Boolean
foreign import _isOdd :: Long -> Boolean
foreign import _isPositive :: Long -> Boolean
foreign import _isZero :: Long -> Boolean
foreign import _lessThan :: Long -> Long -> Boolean
foreign import _lessThanOrEqualTo :: Long -> Long -> Boolean
foreign import _modulo :: Long -> Long -> Long
foreign import _multiply :: Long -> Long -> Long
foreign import _negate :: Long -> Long
foreign import _not :: Long -> Long
foreign import _notEquals :: Long -> Long -> Boolean
foreign import _or :: Long -> Long -> Long
foreign import _shiftLeft :: Long -> Long -> Long
foreign import _shiftRight :: Long -> Long -> Long
foreign import _shiftRightUnsigned :: Long -> Long -> Long
foreign import _rotateLeft :: Long -> Long -> Long
foreign import _rotateRight :: Long -> Long -> Long
foreign import _subtract :: Long -> Long -> Long
foreign import _toBytes :: Long -> Array Int
foreign import _toInt :: Long -> Int
foreign import _toNumber :: Long -> Number
foreign import _toString :: Long -> String
foreign import _xor :: Long -> Long -> Long

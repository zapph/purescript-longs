module Data.Long.Internal
       ( numberBitsToInt
       , safeReadLong
       ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (Radix)
import Data.Long.FFI as FFI
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable

foreign import numberBitsToInt :: Number -> Int

safeReadLong :: String -> FFI.IsUnsigned -> Radix -> Maybe FFI.Long
safeReadLong s isSigned radix =
  Nullable.toMaybe $ runFn3 _safeReadLong s isSigned radix

foreign import _safeReadLong :: Fn3 String FFI.IsUnsigned Radix (Nullable FFI.Long)

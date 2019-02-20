module Data.Long.Internal
       ( numberBitsToInt
       , safeReadLong
       ) where

import Prelude

import Data.Int (Radix)
import Data.Long.FFI as FFI
import Data.Maybe (Maybe(..))
import Effect.Exception (catchException)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)

foreign import numberBitsToInt :: Number -> Int

safeReadLong :: String -> FFI.IsUnsigned -> Radix -> Maybe FFI.Long
safeReadLong s isSigned radix =
  unsafePerformEffect
  $ catchException (\_ -> pure Nothing)
  $ Just <$> runEffectFn3 _safeReadLong s isSigned radix

foreign import _safeReadLong :: EffectFn3 String FFI.IsUnsigned Radix FFI.Long

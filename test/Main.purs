module Test.Main where

import Prelude

import Data.Long.FFITest (testFFI)
import Effect (Effect)

main :: Effect Unit
main = do
  testFFI

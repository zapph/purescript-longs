module Test.Main where

import Prelude

import Data.Long.FFISpec (ffiSpec)
import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] ffiSpec

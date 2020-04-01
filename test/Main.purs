module Test.Main where

import Prelude

import Effect (Effect)
import Test.Record (runRecordTests)
import Test.Variant (runVariantTests)
import Test.HList (runHListTests)

main :: Effect Unit
main = do
  runRecordTests
  runVariantTests
  runHListTests

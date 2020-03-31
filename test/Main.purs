module Test.Main where

import Prelude

import Effect (Effect)
import Test.Record (runRecordTests)

main :: Effect Unit
main = do
  runRecordTests

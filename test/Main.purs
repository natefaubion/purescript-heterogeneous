module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

main :: Effect Unit
main = do
  log "Testing"
  assert true

module Test.Interp where

import Prelude

import Heterogeneous.Folding (class Folding)
import Heterogeneous.Variadic (class Resulting, class Variadic, variadic)

data Interp = Interp

instance Folding Interp String String String where
  folding _ acc a = acc <> a

else instance Show a => Folding Interp String a String where
  folding _ acc a = acc <> show a

instance Resulting Interp String String where
  resulting _ = identity

interp :: forall args. Variadic Interp String args => args
interp = variadic Interp ""

example :: String
example = interp "foo" 42 "baz" false

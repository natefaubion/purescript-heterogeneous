module Test.Variant where

import Prelude

import Data.Variant (Variant)
import Data.Variant as Variant
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Type.Proxy (Proxy(..))

data ShowCase = ShowCase

instance showCase ::
  Show a =>
  Folding ShowCase String a String where
  folding ShowCase s = append s <<< show

type TestLabels =
  ( foo :: Int
  , bar :: Boolean
  , baz :: String
  )

someFoo :: Variant TestLabels
someFoo = Variant.inj (Proxy :: Proxy "foo") 42

showVariantValue :: forall r.
  HFoldl ShowCase String (Variant r) String =>
  Variant r ->
  String
showVariantValue =
  hfoldl ShowCase ""

test :: String
test =
  showVariantValue someFoo

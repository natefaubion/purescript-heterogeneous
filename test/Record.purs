module Test.Record where

import Prelude

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogenous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogenous.Mapping (class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.Row as Row
import Record as Record

newtype ZipProp r = ZipProp { | r }

instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProp fns) (SProxy sym) a b where
  mappingWithIndex (ZipProp fns) prop = Record.get prop fns

zipRecord :: forall rfns rin rout.
  HMapWithIndex (ZipProp rfns) { | rin } { | rout } =>
  { | rfns } ->
  { | rin  } ->
  { | rout }
zipRecord =
  hmapWithIndex <<< ZipProp

testZip :: _
testZip =
  { foo: add 1
  , bar: Tuple "bar"
  , baz: \a -> not a
  }
  `zipRecord`
  { foo: 12
  , bar: 42.0
  , baz: true
  }

data ShowProps = ShowProps

instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (SProxy sym) String a String
  where
  foldingWithIndex _ prop str a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "

showRecord :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  { | r } ->
  String
showRecord r =
  "{ " <> hfoldlWithIndex ShowProps "" r <> " }"

testShow :: _
testShow = showRecord
  { foo: "foo"
  , bar: 42
  , baz: false
  }

data ShowPropsCase = ShowPropsCase

instance showPropsCase_Unit ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowPropsCase (SProxy sym) Unit a String
  where
  foldingWithIndex _ prop _ a =
    reflectSymbol prop <> ": " <> show a

instance showPropsCase_String ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowPropsCase (SProxy sym) String a String
  where
  foldingWithIndex _ prop pre a =
    pre <> ", " <> reflectSymbol prop <> ": " <> show a

showRecordNonEmpty :: forall r.
  HFoldlWithIndex ShowPropsCase Unit { | r } String =>
  { | r } ->
  String
showRecordNonEmpty r =
  "{ " <> hfoldlWithIndex ShowPropsCase unit r <> " }"

testShow2 :: _
testShow2 = showRecordNonEmpty
  { foo: "foo"
  , bar: 42
  , baz: false
  }
-- TypeError:
-- testShow2 = showRecordNonEmpty {}

data AddOneAndShow = AddOneAndShow

instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show

testAddOneAndShow :: _
testAddOneAndShow =
  hmap AddOneAndShow { a: 1, b: 2.0, c: { x: 12, y: 42 } }

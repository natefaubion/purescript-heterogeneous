module Test.Record where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mapping)
import Prim.Row as Row
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Test.Assert (assert, assertEqual, assertEqual')

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

testZip :: Effect Unit
testZip =
  assertEqual
    { actual:
        { foo: add 1
        , bar: Tuple "bar"
        , baz: \a -> not a
        }
        `zipRecord`
        { foo: 12
        , bar: 42.0
        , baz: true
        }
    , expected:
        { foo: 13
        , bar: Tuple "bar" 42.0
        , baz: false
        }
    }


data ShowProps = ShowProps

instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (SProxy sym) String a
  where
  foldingWithIndex _ prop str a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "

showRecord :: forall r.
  HFoldlWithIndex ShowProps String { | r } =>
  { | r } ->
  String
showRecord r =
  "{ " <> hfoldlWithIndex ShowProps "" r <> " }"

testShow :: Effect Unit
testShow =
  assertEqual
    { actual:
        showRecord
          { foo: "foo"
          , bar: 42
          , baz: false
          }
    , expected:
        "{ bar: 42, baz: false, foo: \"foo\" }"
    }


data ShowPropsCase = ShowPropsCase

instance showPropsCase ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowPropsCase (SProxy sym) String a
  where
  foldingWithIndex _ prop acc a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | acc == "" = ""
        | otherwise = acc <> ", "

showRecordNonEmpty :: forall r.
  HFoldlWithIndex ShowPropsCase String { | r } =>
  { | r } ->
  String
showRecordNonEmpty r =
  "{ " <> hfoldlWithIndex ShowPropsCase "" r <> " }"

testShow2 :: Effect Unit
testShow2 =
  assertEqual
    { actual:
        showRecordNonEmpty
          { foo: "foo"
          , bar: 42
          , baz: false
          }
    , expected:
        "{ bar: 42, baz: false, foo: \"foo\" }"
    }
-- TypeError:
-- testShow2 = showRecordNonEmpty {}

data AddOneAndShow = AddOneAndShow

instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show

testAddOneAndShow :: Effect Unit
testAddOneAndShow =
  assertEqual
    { actual:
        hmap AddOneAndShow { a: 1, b: 2.0, c: { x: 12, y: 42 } }
    , expected:
        { a: "2", b: "3.0", c: "{ x: 13, y: 43 }" }
    }


{-
data TraverseProp (f :: Type -> Type) k = TraverseProp k

instance traverseProp ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym b rb rc
  , Mapping k a (f b)
  ) =>
  FoldingWithIndex
    (TraverseProp f k)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    a
  where
  foldingWithIndex (TraverseProp k) prop rin a =
    (>>>) <$> rin <*> (Builder.insert prop <$> mapping k a)

traverseRecord :: forall f k rin rout.
  Applicative f =>
  HFoldlWithIndex (TraverseProp f k) (f (Builder {} { | rout })) { | rin } =>
  k ->
  { | rin } ->
  f { | rout }
traverseRecord k =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (TraverseProp k :: TraverseProp f k) (pure identity :: f (Builder {} {}))

testTraverseRecord :: Effect Unit
testTraverseRecord =
  assertEqual
    { actual:
        traverseRecord (Just :: Int -> Maybe Int)
          { a: 1
          , b: 2
          , c: 3
          }
    , expected:
        (Just { a: 1, b: 2, c: 3 })
    }

data SequencePropOf (f :: Type -> Type) = SequencePropOf

instance sequencePropOf_1 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym a rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    (f a)
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex _ prop rin a =
    (>>>) <$> rin <*> (Builder.insert prop <$> a)
else
instance sequencePropOf_2 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    x
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex _ prop rin x =
    (_ >>> Builder.insert prop x) <$> rin

sequencePropsOf :: forall f rin rout.
  Applicative f =>
  HFoldlWithIndex (SequencePropOf f) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  { | rin } ->
  f { | rout }
sequencePropsOf =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (SequencePropOf :: SequencePropOf f) (pure identity :: f (Builder {} {}))

testSequencePropsOf1 :: Effect Unit
testSequencePropsOf1 =
  assert $
    isNothing $
      sequencePropsOf
        { a: Just "Hello"
        , b: Nothing
        , c: 42
        }

testSequencePropsOf2 :: Effect Unit
testSequencePropsOf2 =
  assertEqual
    { actual:
        sequencePropsOf
          { a: Just "Hello"
          , b: Just 5
          , c: 42
          }
    , expected:
        Just
          { a: "Hello"
          , b: 5
          , c: 42
          }
    }
-}

-----
-- Verify that multiple maps can be used in constraints

newtype ReplaceLeft r = ReplaceLeft { | r }

instance replaceLeftH ::
  (IsSymbol sym, Row.Cons sym a x vals) =>
  MappingWithIndex (ReplaceLeft vals) (SProxy sym) (Either a b) (Either a b) where
  mappingWithIndex (ReplaceLeft vals) prop = lmap (const $ Record.get prop vals)

replaceLeft :: forall rvals rin rout.
  HMapWithIndex (ReplaceLeft rvals) { | rin } { | rout } =>
  { | rvals } ->
  { | rin } ->
  { | rout }
replaceLeft =
  hmapWithIndex <<< ReplaceLeft

testReplaceLeft :: Effect Unit
testReplaceLeft =
  assertEqual'
    "testReplaceLeft"
    { actual:
        { a: "goodbye"
        , b: 100
        }
        `replaceLeft`
        { a: Left "hello"
        , b: Right 1
        }
    , expected:
        { a: Left "goodbye" :: Either String String -- Annotation required for compilation
        , b: Right 1
        }
    }


newtype ReplaceRight r = ReplaceRight { | r }

instance replaceRightH ::
  (IsSymbol sym, Row.Cons sym b x vals) =>
  MappingWithIndex (ReplaceRight vals) (SProxy sym) (Either a b) (Either a b) where
  mappingWithIndex (ReplaceRight vals) prop = map (const $ Record.get prop vals)

replaceRight :: forall rvals rin rout.
  HMapWithIndex (ReplaceRight rvals) { | rin } { | rout } =>
  { | rvals } ->
  { | rin } ->
  { | rout }
replaceRight =
  hmapWithIndex <<< ReplaceRight

testReplaceRight :: Effect Unit
testReplaceRight =
  assertEqual'
    "testReplaceRight"
    { actual:
        { a: "goodbye"
        , b: 100
        }
        `replaceRight`
        { a: Left "hello"
        , b: Right 1
        }
    , expected:
        { a: Left "hello"
        , b: Right 100 :: Either Int Int -- Annotation required for compilation
        }
    }

replaceBoth :: forall rvals r.
  HMapWithIndex (ReplaceLeft rvals) { | r } { | r } =>
  HMapWithIndex (ReplaceRight rvals) { | r } { | r } =>
  { | rvals } ->
  { | r } ->
  { | r }
replaceBoth vals =
  replaceLeft vals >>> replaceRight vals

testReplaceBoth :: Effect Unit
testReplaceBoth =
  assertEqual'
    "testReplaceBoth"
    { actual:
        { a: "goodbye"
        , b: 100
        }
        `replaceBoth`
        { a: Left "hello"
        , b: Right 1
        }
    , expected:
        { a: Left "goodbye" :: Either String String -- Annotation required for compilation
        , b: Right 100 :: Either Int Int -- Annotation required for compilation
        }
    }

-----
-- Verify that multiple folds can be used in constraints.

data CountLeft = CountLeft

instance countLeft :: Folding CountLeft Int (Either a b) where
  folding CountLeft acc (Left _) = acc + 1
  folding CountLeft acc _ = acc

countLefts :: forall r. HFoldl CountLeft Int { | r } => { | r } -> Int
countLefts = hfoldl CountLeft 0

testCountLefts :: Effect Unit
testCountLefts =
  assertEqual'
    "testCountLefts"
    { actual:
        countLefts
          { a: Left "a"
          , b: Right "b"
          , c: Left "c"
          }
    , expected:
        2
    }

data CountRight = CountRight

instance countRight :: Folding CountRight Int (Either a b) where
  folding CountRight acc (Right _) = acc + 1
  folding CountRight acc _ = acc

countRights :: forall r. HFoldl CountRight Int { | r } => { | r } -> Int
countRights = hfoldl CountRight 0

testCountRights :: Effect Unit
testCountRights =
  assertEqual'
    "testCountRights"
    { actual:
        countRights
          { a: Left "a"
          , b: Right "b"
          , c: Left "c"
          }
    , expected:
        1
    }

countBoth :: forall r.
  HFoldl CountLeft Int { | r } =>
  HFoldl CountRight Int { | r } =>
  { | r } ->
  Int
countBoth r = countRights r + countLefts r

testCountBoth :: Effect Unit
testCountBoth =
  assertEqual'
    "testCountBoth"
    { actual:
        countBoth
          { a: Left "a"
          , b: Right "b"
          , c: Left "c"
          }
    , expected:
        3
    }

-----
-- Verify that multiple folds can be used in constraints.

data ShowValues = ShowValues

instance showValues ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowValues (SProxy sym) String a
  where
  foldingWithIndex _ prop str a = pre <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "

showTwice :: forall r.
  HFoldlWithIndex ShowProps String { | r } =>
  HFoldlWithIndex ShowValues String { | r } =>
  { | r } ->
  String
showTwice r = do
  let a = "{ " <> hfoldlWithIndex ShowProps "" r <> " }"
      b = "[ " <> hfoldlWithIndex ShowValues "" r <> " ]"
  a <> b

testShowTwice :: Effect Unit
testShowTwice =
  assertEqual'
    "testShowTwice"
    { actual:
        showTwice
          { foo: "foo"
          , bar: 42
          , baz: false
          }
    , expected:
        "{ bar: 42, baz: false, foo: \"foo\" }[ 42, false, \"foo\" ]"
    }

runRecordTests :: Effect Unit
runRecordTests = do
  testZip
  testShow
  testShow2
  testAddOneAndShow
{-
  testTraverseRecord
  testSequencePropsOf1
  testSequencePropsOf2
-}
  testReplaceLeft
  testReplaceRight
  testReplaceBoth
  testCountLefts
  testCountRights
  testCountBoth
  testShowTwice

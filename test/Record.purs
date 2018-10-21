module Test.Record where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mapping)
import Prim.Row as Row
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder

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
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex (TraverseProp k) prop rin a =
    (>>>) <$> rin <*> (Builder.insert prop <$> mapping k a)

traverseRecord :: forall f k rin rout.
  Applicative f =>
  HFoldlWithIndex (TraverseProp f k) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  k ->
  { | rin } ->
  f { | rout }
traverseRecord k =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (TraverseProp k :: TraverseProp f k) (pure identity :: f (Builder {} {}))

test1 :: _
test1 =
  traverseRecord (Just :: Int -> Maybe Int)
    { a: 1
    , b: 2
    , c: 3
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

test :: Maybe _
test =
  sequencePropsOf
    { a: Just "Hello"
    , b: Nothing
    , c: 42
    }

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

testReplaceLeft :: _
testReplaceLeft =
  { a: "goodbye"
  , b: 100
  }
  `replaceLeft`
  { a: Left "hello"
  , b: Right 1
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

testReplaceRight :: _
testReplaceRight =
  { a: "goodbye"
  , b: 100
  }
  `replaceRight`
  { a: Left "hello"
  , b: Right 1
  }

testReplaceBoth :: forall rvals rin rmid rout.
  HMapWithIndex (ReplaceLeft rvals) { | rin } { | rmid } =>
  HMapWithIndex (ReplaceRight rvals) { | rmid } { | rout } =>
  { | rvals } ->
  { | rin  } ->
  { | rout }
testReplaceBoth vals =
  (replaceLeft vals :: { | rin } -> { | rmid }) >>>
  (replaceRight vals :: { | rmid } -> { | rout })

-----
-- Verify that multiple folds can be used in constraints.

data CountLeft = CountLeft

instance countLeft :: Folding CountLeft Int (Either a b) Int where
  folding CountLeft acc (Left _) = acc + 1
  folding CountLeft acc _ = acc

countLefts :: forall r. HFoldl CountLeft Int { | r } Int => { | r } -> Int
countLefts = hfoldl CountLeft 0

data CountRight = CountRight

instance countRight :: Folding CountRight Int (Either a b) Int where
  folding CountRight acc (Right _) = acc + 1
  folding CountRight acc _ = acc

countRights :: forall r. HFoldl CountRight Int { | r } Int => { | r } -> Int
countRights = hfoldl CountRight 0

countBoth :: forall r.
  HFoldl CountLeft Int { | r } Int =>
  HFoldl CountRight Int { | r } Int =>
  { | r } ->
  Int
countBoth r = countRights r + countLefts r

-----
-- Verify that multiple folds can be used in constraints.

data ShowValues = ShowValues

instance showValues ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowValues (SProxy sym) String a String
  where
  foldingWithIndex _ prop str a = pre <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "

showTwice :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  HFoldlWithIndex ShowValues String { | r } String =>
  { | r } ->
  String
showTwice r = do
  let a = "{ " <> hfoldlWithIndex ShowProps "" r <> " }"
      b = "[ " <> hfoldlWithIndex ShowValues "" r <> " ]"
  a <> b

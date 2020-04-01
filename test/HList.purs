module Test.HList where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, folding, foldingWithIndex, hfoldl, hfoldlWithIndex)
import Test.Assert (assertEqual')

foreign import kind Peano
foreign import data S :: Peano -> Peano
foreign import data Z :: Peano

data Peano (p :: Peano) = Peano

class KnownPeano p where
  reflectPeano :: Peano p -> Int

instance knownPeanoZ :: KnownPeano Z where
  reflectPeano _ = 0

instance knownPeanoS :: KnownPeano p => KnownPeano (S p) where
  reflectPeano _ = 1 + reflectPeano (Peano :: Peano p)

data HCons a b = HCons a b
data HNil = HNil

infixr 8 type HCons as :
infixr 8 HCons as :

newtype Counting (p :: Peano) a = Counting a

instance hfoldlWithIndexCounting_nil :: HFoldlWithIndex f x (Counting p HNil) where
  hfoldlWithIndex _ x _ = x

instance hfoldlWithIndexCounting_go ::
  ( FoldingWithIndex f (Peano p) x a
  , HFoldlWithIndex f x (Counting (S p) b)
  ) =>
  HFoldlWithIndex f x (Counting p (HCons a b))
  where
  hfoldlWithIndex f x (Counting (HCons a b)) = z
    where
    y = foldingWithIndex f (Peano :: Peano p) x a
    z = hfoldlWithIndex f y (Counting b :: Counting (S p) b)

instance hfoldlWithIndexHNil :: HFoldlWithIndex f x HNil where
  hfoldlWithIndex _ x _ = x

instance hfoldlWithIndexHCons ::
  ( FoldingWithIndex f (Peano Z) x a
  , HFoldlWithIndex f x (Counting (S Z) rest)
  ) =>
  HFoldlWithIndex f x (HCons a rest)
  where
  hfoldlWithIndex f x (HCons a rest) = z
    where
    y = foldingWithIndex f (Peano :: Peano Z) x a
    z = hfoldlWithIndex f y (Counting rest :: Counting (S Z) rest)

instance hfoldlHNil :: HFoldl f x HNil where
  hfoldl _ x _ = x
else
instance hfoldlHCons_one ::
  Folding f x a =>
  HFoldl f x (HCons a HNil)
  where
  hfoldl f x (HCons a _) =
    folding f x a
else
instance hfoldlHCons_many ::
  ( Folding f x a
  , HFoldl f x (HCons b c)
  ) =>
  HFoldl f x (HCons a (HCons b c))
  where
  hfoldl f x (HCons a rest) =
    hfoldl f (folding f x a) rest

data ShowWithIndex = ShowWithIndex

instance foldingShowWithIndex ::
  (KnownPeano p, Show a) =>
  FoldingWithIndex ShowWithIndex (Peano p) (Array (Tuple Int String)) a
  where
  foldingWithIndex _ p as a =
    as <> [ Tuple (reflectPeano p) (show a) ]

showWithIndex :: forall hlist.
  HFoldlWithIndex ShowWithIndex (Array (Tuple Int String)) hlist =>
  hlist ->
  Array (Tuple Int String)
showWithIndex =
  hfoldlWithIndex ShowWithIndex ([] :: Array (Tuple Int String))

testShowWithIndex :: Effect Unit
testShowWithIndex =
  assertEqual'
    "testShowWithIndex"
    { actual:
        showWithIndex ("foo" : 42 : true : 12.0 : HNil)
    , expected:
        [(Tuple 0 "\"foo\""),(Tuple 1 "42"),(Tuple 2 "true"),(Tuple 3 "12.0")]
    }

data ShowHList = ShowHList

instance foldingShowHList :: Show a => Folding ShowHList String a where
  folding _ "" a = show a
  folding _ bs a = bs <> " : " <> show a

showHList :: forall hlist.
  HFoldl ShowHList String hlist =>
  hlist ->
  String
showHList h =
  "(" <> hfoldl ShowHList "" h <> " : HNil)"

testShowHList :: Effect Unit
testShowHList =
  assertEqual'
    "testShowHList"
    { actual:
        showHList ("foo" : 42 : true : 12.0 : HNil)
    , expected:
        "(\"foo\" : 42 : true : 12.0 : HNil)"
    }

runHListTests :: Effect Unit
runHListTests = do
  testShowWithIndex
  testShowHList

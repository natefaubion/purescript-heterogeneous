module Heterogenous.Mapping where

import Prelude

import Data.Functor.App (App(..))
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Row (RLProxy(..))

class Mapping f a b | f a -> b where
  mapping :: f -> a -> b

class MappingWithIndex f i a b | f a -> b, f -> i where
  mappingWithIndex :: f -> i -> a -> b

instance mappingFunction :: Mapping (a -> b) a b where
  mapping k = k

newtype ConstMapping f = ConstMapping f

instance constMapping ::
  Mapping f a b =>
  MappingWithIndex (ConstMapping f) ix a b
  where
  mappingWithIndex (ConstMapping f) _ = mapping f

class HMap f a b | a -> f b where
  hmap :: f -> a -> b

class HMapWithIndex f a b | a -> f b where
  hmapWithIndex :: f -> a -> b

instance hmapApp ::
  ( Functor f
  , Mapping fn a b
  ) =>
  HMap fn (App f a) (App f b)
  where
  hmap f (App a) =
    App (mapping f <$> a)

instance hmapWithIndexApp ::
  ( FunctorWithIndex i f
  , MappingWithIndex fn i a b
  ) =>
  HMapWithIndex fn (App f a) (App f b)
  where
  hmapWithIndex f (App a) =
    App (mapWithIndex (mappingWithIndex f) a)

instance hmapRecord ::
  ( RL.RowToList rin rl
  , MapRecordWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn { | rin } { | rout }
  where
  hmap =
    Builder.build
      <<< mapRecordWithIndexBuilder (RLProxy :: RLProxy rl)
      <<< ConstMapping

instance hmapWithIndexRecord ::
  ( RL.RowToList rin rl
  , MapRecordWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn { | rin } { | rout }
  where
  hmapWithIndex =
    Builder.build
      <<< mapRecordWithIndexBuilder (RLProxy :: RLProxy rl)

class MapRecordWithIndex (xs :: RowList) f (as :: # Type) (bs :: # Type) | xs -> f as bs where
  mapRecordWithIndexBuilder :: RLProxy xs -> f -> Builder { | as } { | bs }

instance mapRecordWithIndexCons ::
  ( IsSymbol sym
  , MappingWithIndex f (SProxy sym) a b
  , MapRecordWithIndex rest f as bs'
  , Row.Cons sym a bx bs'
  , Row.Cons sym b bx bs
  ) =>
  MapRecordWithIndex (RL.Cons sym a rest) f as bs
  where
  mapRecordWithIndexBuilder _ f =
    Builder.modify prop (mappingWithIndex f prop)
      <<< mapRecordWithIndexBuilder (RLProxy :: RLProxy rest) f
    where
    prop = SProxy :: SProxy sym

instance mapRecordWithIndexNil :: MapRecordWithIndex RL.Nil fn as as where
  mapRecordWithIndexBuilder _ _ = identity

instance hmapTuple ::
  ( Mapping fn a a'
  , Mapping fn b b'
  ) =>
  HMap fn (Tuple a b) (Tuple a' b')
  where
  hmap fn (Tuple a b) =
    Tuple (mapping fn a) (mapping fn b)

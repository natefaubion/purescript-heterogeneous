module Heterogenous.Mapping where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Row (RLProxy(..))

class Mapping f a b | f a -> b where
  mapping :: f -> a -> b

class MappingWithIndex f i a b | f a -> i b where
  mappingWithIndex :: f -> i -> a -> b

instance mappingFunction :: Mapping (a -> b) a b where
  mapping k = k

data Showing = Showing

instance showMapping :: Show a => Mapping Showing a String where
  mapping _ = show

newtype Injecting f = Injecting (forall a. a -> f a)

instance injMapping :: Mapping (Injecting f) a (f a) where
  mapping (Injecting k) = k

newtype ApplyingProp r = ApplyingProp { | r }

instance applyingPPropMapping ::
  ( IsSymbol sym
  , Row.Cons sym (a -> b) r' r
  ) =>
  MappingWithIndex (ApplyingProp r) (SProxy sym) a b
  where
  mappingWithIndex (ApplyingProp r) prop =
    Record.get prop r

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

newtype App f a = App (f a)

instance hmapApp ::
  ( Functor f
  , Mapping fn a b
  ) =>
  HMap fn (App f a) (App f b)
  where
  hmap f (App a) =
    App (mapping f <$> a)

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

class MapRecordWithIndex (xs :: RowList) f (as :: # Type) (bs :: # Type) | xs f -> as bs where
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

mapRecord :: forall a b rin rout.
  HMap (a -> b) { | rin } { | rout } =>
  (a -> b) ->
  { | rin } ->
  { | rout }
mapRecord = hmap

zipRecord :: forall fns rin rout.
  HMapWithIndex (ApplyingProp fns) { | rin } { | rout } =>
  { | fns } ->
  { | rin } ->
  { | rout }
zipRecord rs =
  hmapWithIndex (ApplyingProp rs)

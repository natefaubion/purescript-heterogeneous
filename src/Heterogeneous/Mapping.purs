module Heterogeneous.Mapping where

import Prelude

import Data.Either (Either(..))
import Data.Functor.App (App(..))
import Data.Functor.Variant (FProxy, VariantF)
import Data.Functor.Variant as VariantF
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
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

class HMap f a b | f a -> b where
  hmap :: f -> a -> b

class HMapWithIndex f a b | f a -> b where
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

instance hmapEither ::
  ( Mapping fn a a'
  , Mapping fn b b'
  ) =>
  HMap fn (Either a b) (Either a' b')
  where
  hmap fn = case _ of
    Left a -> Left (mapping fn a)
    Right b -> Right (mapping fn b)

instance hmapVariant ::
  ( RL.RowToList rin rl
  , MapVariantWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn (Variant rin) (Variant rout)
  where
  hmap =
    mapVariantWithIndex (RLProxy :: RLProxy rl) <<< ConstMapping

instance hmapWithIndexVariant ::
  ( RL.RowToList rin rl
  , MapVariantWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn (Variant rin) (Variant rout)
  where
  hmapWithIndex =
    mapVariantWithIndex (RLProxy :: RLProxy rl)

class MapVariantWithIndex (xs :: RowList) f (as :: # Type) (bs :: # Type) | xs -> f as bs where
  mapVariantWithIndex :: RLProxy xs -> f -> Variant as -> Variant bs

instance mapVariantWithIndexCons ::
  ( IsSymbol sym
  , Row.Cons sym a r1 r2
  , Row.Cons sym b r3 r4
  , MappingWithIndex fn (SProxy sym) a b
  , MapVariantWithIndex rest fn r1 r4
  ) =>
  MapVariantWithIndex (RL.Cons sym a rest) fn r2 r4
  where
  mapVariantWithIndex _ fn =
    mapVariantWithIndex (RLProxy :: RLProxy rest) fn
      # Variant.on label (Variant.inj label <<< mappingWithIndex fn label)
    where
    label = SProxy :: SProxy sym

instance mapVariantWithIndexNil :: MapVariantWithIndex RL.Nil fn () r where
  mapVariantWithIndex _ _ = Variant.case_

instance hmapVariantF ::
  ( RL.RowToList rin rl
  , MapVariantFWithIndex rl (ConstMapping fn) rin rout x y
  ) =>
  HMap fn (VariantF rin x) (VariantF rout y)
  where
  hmap =
    mapVariantFWithIndex (RLProxy :: RLProxy rl) <<< ConstMapping

instance hmapWithIndexVariantF ::
  ( RL.RowToList rin rl
  , MapVariantFWithIndex rl fn rin rout x y
  ) =>
  HMapWithIndex fn (VariantF rin x) (VariantF rout y)
  where
  hmapWithIndex =
    mapVariantFWithIndex (RLProxy :: RLProxy rl)

class MapVariantFWithIndex (xs :: RowList) f (as :: # Type) (bs :: # Type) x y | xs -> f as bs x y where
  mapVariantFWithIndex :: RLProxy xs -> f -> VariantF as x -> VariantF bs y

instance mapVariantFWithIndexCons ::
  ( IsSymbol sym
  , Row.Cons sym (FProxy a) r1 r2
  , Row.Cons sym (FProxy b) r3 r4
  , MappingWithIndex fn (SProxy sym) (a x) (b y)
  , MapVariantFWithIndex rest fn r1 r4 x y
  , Functor b
  ) =>
  MapVariantFWithIndex (RL.Cons sym (FProxy a) rest) fn r2 r4 x y
  where
  mapVariantFWithIndex _ fn =
    mapVariantFWithIndex (RLProxy :: RLProxy rest) fn
      # VariantF.on label (VariantF.inj label <<< mappingWithIndex fn label)
    where
    label = SProxy :: SProxy sym

instance mapVariantFWithIndexNil :: MapVariantFWithIndex RL.Nil fn () r x y where
  mapVariantFWithIndex _ _ = VariantF.case_

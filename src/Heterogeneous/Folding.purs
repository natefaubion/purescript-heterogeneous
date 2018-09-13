module Heterogeneous.Folding where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Functor.App (App(..))
import Data.Functor.Variant (FProxy, VariantF)
import Data.Functor.Variant as VariantF
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))

class Folding f x y z | f x y -> z where
  folding :: f -> x -> y -> z

instance functionFolding :: Folding (x -> y -> x) x y x where
  folding f = f

class FoldingWithIndex f i x y z | f x y -> z, f -> i where
  foldingWithIndex :: f -> i -> x -> y -> z

instance functionFoldingWithIndex :: FoldingWithIndex (i -> x -> y -> x) i x y x where
  foldingWithIndex f = f

class HFoldl f x a b | a -> f x b where
  hfoldl :: f -> x -> a -> b

class HFoldlWithIndex f x a b | a -> f x b where
  hfoldlWithIndex :: f -> x -> a -> b

newtype ConstFolding f = ConstFolding f

instance constFolding ::
  ( Folding f x y z
  ) =>
  FoldingWithIndex (ConstFolding f) i x y z
  where
  foldingWithIndex (ConstFolding f) _ = folding f

instance hfoldlApp ::
  ( Foldable g
  , Folding f x y x
  ) =>
  HFoldl f x (App g y) x
  where
  hfoldl f x (App g) =
    foldl (folding f) x g

instance hfoldlWithIndexApp ::
  ( FoldableWithIndex i g
  , FoldingWithIndex f i x y x
  ) =>
  HFoldlWithIndex f x (App g y) x
  where
  hfoldlWithIndex f x (App g) =
    foldlWithIndex (foldingWithIndex f) x g

instance hfoldlRowList ::
  ( HFoldlWithIndex (ConstFolding f) x (RLProxy rl) b
  ) =>
  HFoldl f x (RLProxy rl) b
  where
  hfoldl f =
    hfoldlWithIndex (ConstFolding f)

instance hfoldlWithIndexRowListCons ::
  ( FoldingWithIndex f (SProxy sym) x (Proxy y) z
  , HFoldlWithIndex f z (RLProxy rl) b
  ) =>
  HFoldlWithIndex f x (RLProxy (RL.Cons sym y rl)) b
  where
  hfoldlWithIndex f x _ =
    hfoldlWithIndex f
      (foldingWithIndex f (SProxy :: SProxy sym) x (Proxy :: Proxy y))
      (RLProxy :: RLProxy rl)

instance hfoldlWithIndexRowListNil ::
  HFoldlWithIndex f x (RLProxy RL.Nil) x
  where
  hfoldlWithIndex f x _ = x

instance hfoldlRecord ::
  ( RL.RowToList r rl
  , FoldlRecord (ConstFolding f) x rl r b
  ) =>
  HFoldl f x { | r } b
  where
  hfoldl f x =
    foldlRecordRowList (ConstFolding f) x (RLProxy :: RLProxy rl)

instance hfoldlRecordWithIndex ::
  ( RL.RowToList r rl
  , FoldlRecord f x rl r b
  ) =>
  HFoldlWithIndex f x { | r } b
  where
  hfoldlWithIndex f x =
    foldlRecordRowList f x (RLProxy :: RLProxy rl)

class FoldlRecord f x (rl :: RowList) (r :: # Type) b | rl -> f x r b where
  foldlRecordRowList :: f -> x -> RLProxy rl -> { | r } -> b

instance foldlRecordCons ::
  ( IsSymbol sym
  , Row.Cons sym a r' r
  , FoldingWithIndex f (SProxy sym) x a z
  , FoldlRecord f z rl r b
  ) =>
  FoldlRecord f x (RL.Cons sym a rl) r b
  where
  foldlRecordRowList f x _ r =
    foldlRecordRowList f
      (foldingWithIndex f prop x (Record.get prop r)) (RLProxy :: RLProxy rl)
      r
    where
    prop = SProxy :: SProxy sym

instance foldlRecordNil ::
  FoldlRecord f x RL.Nil r x
  where
  foldlRecordRowList f x _ r = x

instance hfoldlTuple ::
  ( Folding f x a y
  , Folding f y b z
  ) =>
  HFoldl f x (Tuple a b) z
  where
  hfoldl f x (Tuple a b) =
    folding f (folding f x a) b

instance hfoldlEither ::
  ( Folding f x a y
  , Folding f x b y
  ) =>
  HFoldl f x (Either a b) y
  where
  hfoldl f x = case _ of
    Left a -> folding f x a
    Right b -> folding f x b

instance hfoldlVariant ::
  ( RL.RowToList r rl
  , FoldlVariant (ConstFolding f) x rl r y
  ) =>
  HFoldl f x (Variant r) y
  where
  hfoldl =
    foldlVariantRowList (RLProxy :: RLProxy rl) <<< ConstFolding

instance hfoldlVariantWithIndex ::
  ( RL.RowToList r rl
  , FoldlVariant f x rl r y
  ) =>
  HFoldlWithIndex f x (Variant r) y
  where
  hfoldlWithIndex =
    foldlVariantRowList (RLProxy :: RLProxy rl)

class FoldlVariant f x (rl :: RowList) (r :: # Type) b | rl -> f x r b where
  foldlVariantRowList :: RLProxy rl -> f -> x -> Variant r -> b

instance foldlVariantCons ::
  ( IsSymbol sym
  , Row.Cons sym a r1 r2
  , FoldingWithIndex f (SProxy sym) x a y
  , FoldlVariant f x rest r1 y
  ) =>
  FoldlVariant f x (RL.Cons sym a rest) r2 y
  where
  foldlVariantRowList _ f x =
    foldlVariantRowList (RLProxy :: RLProxy rest) f x
      # Variant.on label (foldingWithIndex f label x)
    where
    label = SProxy :: SProxy sym

instance foldlVariantNil :: FoldlVariant f x RL.Nil () y where
  foldlVariantRowList _ _ _ = Variant.case_

instance hfoldlVariantF ::
  ( RL.RowToList r rl
  , FoldlVariantF (ConstFolding f) x rl r z y
  ) =>
  HFoldl f x (VariantF r z) y
  where
  hfoldl =
    foldlVariantFRowList (RLProxy :: RLProxy rl) <<< ConstFolding

instance hfoldlVariantFWithIndex ::
  ( RL.RowToList r rl
  , FoldlVariantF f x rl r z y
  ) =>
  HFoldlWithIndex f x (VariantF r z) y
  where
  hfoldlWithIndex =
    foldlVariantFRowList (RLProxy :: RLProxy rl)

class FoldlVariantF f x (rl :: RowList) (r :: # Type) z y | rl -> f x r z y where
  foldlVariantFRowList :: RLProxy rl -> f -> x -> VariantF r z -> y

instance foldlVariantFCons ::
  ( IsSymbol sym
  , Row.Cons sym (FProxy a) r1 r2
  , FoldingWithIndex f (SProxy sym) x (a z) y
  , FoldlVariantF f x rest r1 z y
  ) =>
  FoldlVariantF f x (RL.Cons sym (FProxy a) rest) r2 z y
  where
  foldlVariantFRowList _ f x =
    foldlVariantFRowList (RLProxy :: RLProxy rest) f x
      # VariantF.on label (foldingWithIndex f label x)
    where
    label = SProxy :: SProxy sym

instance foldlVariantFNil :: FoldlVariantF f x RL.Nil () z y where
  foldlVariantFRowList _ _ _ = VariantF.case_

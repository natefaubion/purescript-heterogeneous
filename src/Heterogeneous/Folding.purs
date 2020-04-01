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
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class Folding f x y where
  folding :: f -> x -> y -> x

instance functionFolding :: Folding (x -> y -> x) x y where
  folding f = f

class FoldingWithIndex f i x y where
  foldingWithIndex :: f -> i -> x -> y -> x

instance functionFoldingWithIndex :: FoldingWithIndex (i -> x -> y -> x) i x y where
  foldingWithIndex f = f

class HFoldl f x a where
  hfoldl :: f -> x -> a -> x

class HFoldlWithIndex f x a where
  hfoldlWithIndex :: f -> x -> a -> x

newtype ConstFolding f = ConstFolding f

instance constFolding ::
  ( Folding f x y
  ) =>
  FoldingWithIndex (ConstFolding f) i x y
  where
  foldingWithIndex (ConstFolding f) _ = folding f

instance hfoldlApp ::
  ( Foldable g
  , Folding f x y
  ) =>
  HFoldl f x (App g y)
  where
  hfoldl f x (App g) =
    foldl (folding f) x g

instance hfoldlWithIndexApp ::
  ( FoldableWithIndex i g
  , FoldingWithIndex f i x y
  ) =>
  HFoldlWithIndex f x (App g y)
  where
  hfoldlWithIndex f x (App g) =
    foldlWithIndex (foldingWithIndex f) x g

instance hfoldlRowList ::
  ( HFoldlWithIndex (ConstFolding f) x (RLProxy rl)
  ) =>
  HFoldl f x (RLProxy rl)
  where
  hfoldl f =
    hfoldlWithIndex (ConstFolding f)

instance hfoldlWithIndexRowListCons ::
  ( FoldingWithIndex f (SProxy sym) x (Proxy y)
  , HFoldlWithIndex f x (RLProxy rl)
  ) =>
  HFoldlWithIndex f x (RLProxy (RL.Cons sym y rl))
  where
  hfoldlWithIndex f x _ =
    hfoldlWithIndex f
      (foldingWithIndex f (SProxy :: SProxy sym) x (Proxy :: Proxy y))
      (RLProxy :: RLProxy rl)

instance hfoldlWithIndexRowListNil ::
  HFoldlWithIndex f x (RLProxy RL.Nil)
  where
  hfoldlWithIndex f x _ = x

instance hfoldlRecord ::
  ( RL.RowToList r rl
  , FoldlRecord (ConstFolding f) x rl r
  ) =>
  HFoldl f x { | r }
  where
  hfoldl f x =
    foldlRecordRowList (ConstFolding f) x (RLProxy :: RLProxy rl)

instance hfoldlRecordWithIndex ::
  ( RL.RowToList r rl
  , FoldlRecord f x rl r
  ) =>
  HFoldlWithIndex f x { | r }
  where
  hfoldlWithIndex f x =
    foldlRecordRowList f x (RLProxy :: RLProxy rl)

class FoldlRecord f x (rl :: RowList) (r :: # Type) | rl -> r where
  foldlRecordRowList :: f -> x -> RLProxy rl -> { | r } -> x

instance foldlRecordCons ::
  ( IsSymbol sym
  , Row.Cons sym a r' r
  , FoldingWithIndex f (SProxy sym) x a
  , FoldlRecord f x rl r
  ) =>
  FoldlRecord f x (RL.Cons sym a rl) r
  where
  foldlRecordRowList f x _ r =
    foldlRecordRowList f
      (foldingWithIndex f prop x (Record.get prop r)) (RLProxy :: RLProxy rl)
      r
    where
    prop = SProxy :: SProxy sym

instance foldlRecordNil ::
  FoldlRecord f x RL.Nil r
  where
  foldlRecordRowList _ x _ _ = x

instance hfoldlTuple ::
  ( Folding f x a
  , Folding f x b
  ) =>
  HFoldl f x (Tuple a b)
  where
  hfoldl f x (Tuple a b) =
    folding f (folding f x a) b

instance hfoldlEither ::
  ( Folding f x a
  , Folding f x b
  ) =>
  HFoldl f x (Either a b)
  where
  hfoldl f x = case _ of
    Left a -> folding f x a
    Right b -> folding f x b

instance hfoldlVariant ::
  ( RL.RowToList r rl
  , FoldlVariant (ConstFolding f) x rl r
  ) =>
  HFoldl f x (Variant r)
  where
  hfoldl =
    foldlVariantRowList (RLProxy :: RLProxy rl) <<< ConstFolding

instance hfoldlVariantWithIndex ::
  ( RL.RowToList r rl
  , FoldlVariant f x rl r
  ) =>
  HFoldlWithIndex f x (Variant r)
  where
  hfoldlWithIndex =
    foldlVariantRowList (RLProxy :: RLProxy rl)

class FoldlVariant f x (rl :: RowList) (r :: # Type) | rl -> r where
  foldlVariantRowList :: RLProxy rl -> f -> x -> Variant r -> x

instance foldlVariantCons ::
  ( IsSymbol sym
  , Row.Cons sym a r1 r2
  , FoldingWithIndex f (SProxy sym) x a
  , FoldlVariant f x rest r1
  ) =>
  FoldlVariant f x (RL.Cons sym a rest) r2
  where
  foldlVariantRowList _ f x =
    foldlVariantRowList (RLProxy :: RLProxy rest) f x
      # Variant.on label (foldingWithIndex f label x)
    where
    label = SProxy :: SProxy sym

instance foldlVariantNil :: FoldlVariant f x RL.Nil () where
  foldlVariantRowList _ _ _ = Variant.case_

instance hfoldlVariantF ::
  ( RL.RowToList r rl
  , FoldlVariantF (ConstFolding f) x rl r z
  ) =>
  HFoldl f x (VariantF r z)
  where
  hfoldl =
    foldlVariantFRowList (RLProxy :: RLProxy rl) <<< ConstFolding

instance hfoldlVariantFWithIndex ::
  ( RL.RowToList r rl
  , FoldlVariantF f x rl r z
  ) =>
  HFoldlWithIndex f x (VariantF r z)
  where
  hfoldlWithIndex =
    foldlVariantFRowList (RLProxy :: RLProxy rl)

class FoldlVariantF f x (rl :: RowList) (r :: # Type) z | f x rl z -> r where
  foldlVariantFRowList :: RLProxy rl -> f -> x -> VariantF r z -> x

instance foldlVariantFCons ::
  ( IsSymbol sym
  , Row.Cons sym (FProxy a) r1 r2
  , FoldingWithIndex f (SProxy sym) x (a z)
  , FoldlVariantF f x rest r1 z
  ) =>
  FoldlVariantF f x (RL.Cons sym (FProxy a) rest) r2 z
  where
  foldlVariantFRowList _ f x =
    foldlVariantFRowList (RLProxy :: RLProxy rest) f x
      # VariantF.on label (foldingWithIndex f label x)
    where
    label = SProxy :: SProxy sym

instance foldlVariantFNil :: FoldlVariantF f x RL.Nil () z where
  foldlVariantFRowList _ _ _ = VariantF.case_

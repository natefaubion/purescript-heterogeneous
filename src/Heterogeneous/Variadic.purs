module Heterogeneous.Variadic where

import Heterogeneous.Folding (class Folding, class FoldingWithIndex, folding, foldingWithIndex)
import Prim.Int (class Add)
import Type.Proxy (Proxy(..))

class Variadic f acc args where
  variadic :: f -> acc -> args

instance variadicArg ::
  ( Folding f acc x acc'
  , Variadic f acc' xs
  ) =>
  Variadic f acc (x -> xs) where
  variadic f acc x =
    variadic f (folding f acc x)

else instance variadicResult ::
  ( Resulting f acc x
  ) =>
  Variadic f acc x where
  variadic = resulting

class Resulting f acc x where
  resulting :: f -> acc -> x

class VariadicWithIndex f acc args where
  variadicWithIndex :: f -> acc -> args

instance variadicWithIndexInit ::
  ( VariadicWithIndexAt f 0 acc args
  ) =>
  VariadicWithIndex f acc args where
  variadicWithIndex f = variadicWithIndexAt f (Proxy :: _ 0)

class VariadicWithIndexAt f (ix :: Int) acc args where
  variadicWithIndexAt :: f -> Proxy ix -> acc -> args

instance variadicWithIndexArg ::
  ( FoldingWithIndex f (Proxy ix) acc x acc'
  , VariadicWithIndexAt f ix' acc' xs
  , Add ix 1 ix'
  ) =>
  VariadicWithIndexAt f ix acc (x -> xs) where
  variadicWithIndexAt f ix acc x =
    variadicWithIndexAt f (Proxy :: _ ix') (foldingWithIndex f ix acc x)

else instance variadicWithIndexResult ::
  ( ResultingWithLength f (Proxy n) acc x
  ) =>
  VariadicWithIndexAt f n acc x where
  variadicWithIndexAt = resultingWithLength

class ResultingWithLength f n acc x where
  resultingWithLength :: f -> n -> acc -> x

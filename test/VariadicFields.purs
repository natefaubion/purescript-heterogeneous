module Test.NFields where

import Data.Symbol (class IsSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Heterogeneous.Variadic (class ResultingWithLength, class VariadicWithIndex, variadicWithIndex)
import Prim.Int (class ToString)
import Prim.Row as Row
import Prim.Symbol (class Append)
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))

data Fields (base :: Symbol) = Fields

instance
  ( ToString ix ixsym
  , Append base ixsym sym
  , IsSymbol sym
  , Row.Lacks sym rin
  , Row.Cons sym a rin rout
  ) =>
  FoldingWithIndex (Fields base) (Proxy ix) { | rin } a { | rout } where
  foldingWithIndex _ _ r a =
    Record.insert (Proxy :: _ sym) a r

instance
  ( TypeEquals { length :: Proxy len | rin } rout
  , Row.Lacks "length" rin
  ) =>
  ResultingWithLength (Fields base) (Proxy len) { | rin } rout where
  resultingWithLength _ len r =
    to (Record.insert (Proxy :: _ "length") len r)

make :: forall @base args. VariadicWithIndex (Fields base) {} args => args
make = variadicWithIndex (Fields @base) {}

example :: Record _
example = make @"slot" 1 true "bar" [ 1 ]

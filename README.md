# purescript-heterogeneous

[![Latest release](http://img.shields.io/github/release/natefaubion/purescript-heterogeneous.svg)](https://github.com/natefaubion/purescript-heterogeneous/releases)
[![Build status](https://github.com/natefaubion/purescript-heterogeneous/workflows/CI/badge.svg?branch=master)](https://github.com/natefaubion/purescript-heterogeneous/actions?query=workflow%3ACI+branch%3Amaster)

Maps and folds for heterogeneous data types.

## Why?

PureScript has a very rich type-system which lets us explicitly describe the
shape of our data in fine detail. For example, Records and row-types let us
write ergonomic, polymorphic, structurally-typed data. However, writing generic
operations over such data types often involves a lot of tedious, constraint-level
tricks. This library provides a framework which separates the traversal and
summary logic for heterogeneous data types, like `Functor` and `Foldable` do for
homogeneous types, so we can reduce the boilerplate and tricks while sharing
implementations.

## How to use this library

This library exports several classes for both indexed and unindexed folds and maps.

* `class HMap` and `hmap`
* `class HMapWithIndex` and `hmapWithIndex`
* `class HFoldl` and `hfoldl`
* `class HFoldlWithIndex` and `hfoldlWithIndex`

These are similar to their homogeneous counterparts. In fact, the folds and maps we
write for heterogeneous types can be reused for homogeneous data via the `App f a`
newtype. That is, the following are identical:

```purescript
map f [1, 2, 3]
```
```purescript
hmap f (App [1, 2, 3])
```

Normal functions aren't enough for a lot of the things we need to do though, so
dispatching these is slightly different as we'll need to use PureScript's constraint
system.

The following examples will operate over `Record` types, since it is the most
ubiquitous heterogeneous data type in PureScript.

### Example: Mapping over a homogeneous Record

Records aren't ever actually homogeneous to the type system, but sometimes all the
values end up being the same type. Mapping over an apparently homogeneous `Record`
is as simple as using a normal (monomorphic) function.

```purescript
hmap (add 1 >>> show) { a: 1, b: 2, c: 3 }
```
```
{ a: "2", b: "3", c: "4" }
```

### Example: Mapping over a heterogeneous Record

In the previous example, we can generalize our mapping function:

```purescript
addOneAndShow :: forall n. Semiring n => Show n => n -> String
addOneAndShow = add one >>> show
```

However, we won't be able to dispatch this without giving it a monomorphic type
signature which fixes it to a homogeneous `Record` like before. We want to be able to
instantiate these dictionaries for each member individually. To do that, we first
need a data type to represent our mapping function:

```purescript
data AddOneAndShow = AddOneAndShow
```

With this we can define an instance of `Mapping`, which we can use with `hmap`.

```purescript
instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show
```

Then instead of passing a function to `hmap`, we pass in our data type.

```purescript
hmap AddOneAndShow { a: 1, b: 2.0, c: { x: 12, y: 42 } }
```
```
{ a: "2", b: "3.0", c: "{ x: 13, y: 43 }" }
```

### Example: Mapping over a heterogeneous Record with additional arguments

With normal functions, we can use partial application to thread in additional
context for our mapping function. We can do the same here by adding arguments
to our mapping data type.

Say we want to implement a `Record` zip operation. That is, we want to map over
a record, and for each field we want to look up a corresponding function in
_another_ record and apply it to the value.

First we need to define a data type for our zip mapping, but we want it to hold
the record of functions:

```purescript
newtype ZipProps fns = ZipProps { | fns }
```

If we use `MappingWithIndex` instead of `Mapping` we can utilize the field name
as well via `Proxy`.

```purescript
instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (Proxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns
```

```purescript
let
  zipRecord = hmapWithIndex <<< ZipProps
in
  { a: add 1
  , b: Tuple "bar"
  , c: \a -> not a
  }
  `zipRecord`
  { a: 12
  , b: 42.0
  , c: true
  }
```
```
{ a: 13, b: (Tuple "bar" 42.0), c: false }
```

### Example: Folding over a homogeneous Record

Much like with `hmap`, we can fold over homogeneous records using normal functions.

```purescript
hfoldl (add :: Int -> Int -> Int) 0 { a: 12, b: 42, c: 100 }
```
```
154
```

The homogeneous case needs a monomorphic function, so we've specialized the type of
`add` with a type signature.

### Example: Folding over a heterogeneous Record

In this example we will implement an alternative `Show` instance using String
concatenation instead of building an intermediate data structure. First, the
data type for our fold function:

```purescript
data ShowProps = ShowProps
```

We need to follow the same steps as we did with `hmap` and `hmapWithIndex`,
except we will implement `Folding` or `FoldingWithIndex`.

```purescript
instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (Proxy sym) String a String where
  foldingWithIndex ShowProps prop str a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "
```

Then we can write a wrapper, which adds the curly-braces:

```purescript
showRecord :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  { | r } ->
  String
showRecord r =
  "{ " <> hfoldlWithIndex ShowProps "" r <> " }"
```

```purescript
showRecord { a: "foo" , b: 42 , c: false }
```
```
"{ a: \"foo\", b: 42, c: false }"
```

## Helping type inference along

The compiler will not always be able to infer all types for the maps and folds
we write for heterogeneous types. That's because it will attempt to determine
an output from any combination of folding function, accumulator, and input (for
folds) or mapping function and input (for maps). This ensures that multiple
mapping and folding operations can be supported for the same underlying input,
but has the downside that the compiler will not infer these types.

You will need to provide annotations for any folding, mapping, accumulator, and
input types that are not determined in some other way.

For example, this sample `showWithIndex` function for showing a heterogeneous
list requires an annotation for the accumulator type:

```purescript
showWithIndex :: forall hlist.
  HFoldlWithIndex ShowWithIndex (Array (Tuple Int String)) hlist (Array (Tuple Int String)) =>
  hlist ->
  Array (Tuple Int String)
showWithIndex =
  hfoldlWithIndex ShowWithIndex ([] :: Array (Tuple Int String))
```

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-heterogeneous).

# Lorentz: adopting complex objects to Michelson

This is the second post in a series about Lorentz — a Haskell eDSL for [Michelson](https://gitlab.com/camlcase-dev/michelson-tutorial/blob/master/README.md) smart contracts language.
[The first post](https://serokell.io/blog/lorentz-implementing-smart-contract-edsl-in-haskell) laid the groundwork for Lorentz as a language very similar to Michelson but written in Haskell.
At this stage, our prototype is yet inconvenient for implementing actual contracts.


One of the major features we would like to have is support for objects with multiple fields.
This should be similar to structs in C or ADT in Haskell, though it would serve only for grouping data in a manageable way.

In this post, we are going to implement complex product and sum types and methods for working with them while ensuring correctness at compile-time. We will explain what Haskell Generics feature is and how it can be used to implement such sort of functionality.

## Problem

[In the previous post](https://serokell.io/blog/lorentz-implementing-smart-contract-edsl-in-haskell), we introduced the Lorentz base syntax which allows one to write code as follows:

```haskell
sumUp :: '[Integer, Integer, Integer] :-> '[Integer]
sumUp = do
  add
  mul
```

At this point, we yet inherit all of the Michelson types and instructions.
This includes some types well-known to ones who have worked with functional languages:

* `pair a b` type, inhabited with `Pair x y`;
* `or a b`, inhabited with either `Left x` or `Right y`;
* `unit` type, inhabited with `Unit`.

Though, being a low-level language, it does not support whole sum types and objects, leaving this for higher-level languages like [Liquidity](https://www.liquidity-lang.org/) and [LIGO](https://ligolang.org/).
As the absence of objects becomes a big inconvenience already for moderately sized contracts, we would like to provide support for such constructions in Lorentz.

## Translating datatypes

Those `pair` and `or` are very similar to building blocks used in [`Generics`](http://hackage.haskell.org/package/base-4.13.0.0/docs/GHC-Generics.html), so it seems natural to try them here.

Shortly speaking, Generics provide a way to decompose datatypes into a sum of products with uniform representation.
Neglecting some of the details, the latter contains the following primitive building blocks:
* `a :*: b`, inhabited with `x :*: y`;
* `a :+: b`, inhabited with `L1 x` or `R1 y`;
* `U1`, inhabited with `U1`;
* `V1`, uninhabited;
* `Rec0` — immediate wrapper for a field, has constructor called `K1`.

For instance, if you have a datatype like:

```haskell
data MyType Int Double String ()
  deriving Generic
```

it will have the following representation (hiding some non-significant details at the moment):

```haskell
>>> import GHC.Generics

>>> :kind! Rep MyType
(Rec0 Int :*: Rec0 Double) :*: (Rec0 String :*: Rec0 ())

>>> from (MyType 1 2.0 "a" ())
(K1 1 :*: K1 2.0) :*: (K1 "a" :*: K1 ())

>>> :kind! Rep ()
U1

>>> from ()
U1
```

So, product types are represented as a tree of `:*:`s, and `:+:` is similarly used for sum types.
These trees get automatically balanced, which is good as this will allow for getters and setters with better average-case complexity.

Now, we want to use generics for implementing `IsoValue`.
Going by the book, we define a typeclass for traversing the generic representation of a type.

```haskell
class GIsoValue (x :: Type -> Type) where
  type GToT x :: T
  gToVal :: x p -> Val t
  gFromVal :: Val t -> x p
```

And then we describe how this representation associates with Michelson primitives:

```haskell
-- | Product type.
-- Each node in generic's binary tree corresponds to "pair" type
-- in Michelson.
instance (GIsoValue x, GIsoValue y) => GIsoValue (x :*: y) where
  type GToT (x :*: y) = 'TPair (GToT x) (GToT y)
  gToVal (x :*: y) = VPair (gToVal x) (gToVal y)
  gFromVal (VPair x y) = gFromVal x :*: gFromVal y

-- | Sum type.
instance (GIsoValue x, GIsoValue y) => GIsoValue (x :+: y) where
  type GToT (x :+: y) = 'TOr (GToT x) (GToT y)
  gToVal = VOr . \case
    L1 x -> VOr (Left $ gToVal x)
    R1 y -> VOr (Right $ gToVal y)
  gFromVal (VOr e) = case e of
    Left x -> L1 (gFromVal x)
    Right y -> R1 (gFromVal y)

-- | Unit type.
instance GIsoValue U1 where
  type GToT U1 = 'TUnit
  gToVal U1 = VUnit
  gFromVal VUnit = U1

-- | Leaf in tree.
-- Here we delegate to inner 'IsoValue', not 'GIsoValue', because user
-- may want to have a custom 'IsoValue' definition for his inner type.
instance IsoValue a => GIsoValue (Rec0 a) where
  type GToT (Rec0 a) = ToT a
  gToVal (K1 a) = toVal a
  gFromVal a = K1 (fromVal a)

-- | Wrappers with meta information which we don't care about.
instance GIsoValue x => GIsoValue (M1 t i x) where
  type GToT (M1 t i x) = GToT x
  gToVal = gToVal . unM1
  gFromVal = M1 . gFromVal
```

`Void` and similar types that are not inhabited cannot be represented in Michelson yet, so we define a dummy instance which prompts this fact on usage attempt (this way, we override the default "cannot deduce instance" error).

```haskell
instance TypeError ('Text "Michelson forbids void-like types") =>
         GIsoValue V1 where
  type GToT V1 = TypeError ('Text "Attempt to use void-like type")
  gToVal = error "impossible"
  gFromVal = error "impossible"
```

Note: empty types cannot be represented in Michelson only at the moment of writing, this feature might have been released already, see [the official Tezos repository](https://gitlab.com/tezos/tezos/issues/662).

In the majority of cases, one will probably want to use these derivation rules when writing an `IsoValue` instance for a datatype.
Hence, we would like to set `GIsoValue` as the default implementation for `IsoValue`.

```haskell
import qualified Generic as G

class IsoValue a where
  type ToT a :: T
  type ToT a = GToT (G.Rep a)

  toVal :: a -> Val (ToT a)
  default toVal
    :: (Generic a, GIsoValue (G.Rep a), ToT a ~ GToT (G.Rep a))
    => a -> Val (ToT a)
  toVal = gToVal . G.from

  fromVal :: Val (ToT a) -> a
  default fromVal
    :: (Generic a, GIsoValue (G.Rep a), ToT a ~ GToT (G.Rep a))
    => Value (ToT a) -> a
  fromVal = G.to . gFromVal
```

Now, a contract developer can write something like this:

```haskell
{-# LANGUAGE DerivingStrategies #-}

data MyType
  = Ctor1 Integer Natural
  | Ctor2
  deriving stock Generic
  deriving anyclass IsoValue

-- This type ^ will be represented as "or (pair int nat) unit"
-- in Michelson.

put1 :: s :-> MyType : s
put1 = push (Ctor1 1 2)  -- translates to "PUSH (Left (Pair 1 2))"

put2 :: s :-> MyType : s
put2 = push Ctor2  -- translates to "PUSH (Right Unit)"
```

### Macros for working with objects

#### Problem

One of the places where Lorentz becomes extremely useful is methods for working with user-defined types.

Upon starting with our first real production contract, we were scared of the necessity to write code like:

```haskell
type Storage = Storage
  { admin :: Address
  , paused :: Bool
  , proxy :: Address
  , totalSupply :: Natural
  , participantsNum :: Natural
  }

someMethod = do
  stackType @[Storage]

  -- get admin field
  dup; cdr; car            -- <- sad
  sender; assertEq ...

  -- get proxy field
  dup; cdr; cdr; cdr; car  -- <- even more sad
  not; assertEq ...
```

Writing such code is inconvenient, but this inconvenience can barely compete with the pain related to modification costs, when one, for instance, needs to add a new field to their storage.
We wanted to put the burden of building the exact sequence of `car`s and `cdr`s on our eDSL, as many other high-level languages over Michelson do as part of their supplied feature set.

#### Implementation example

Let's see how the simplest method — field getter — can be implemented.
We need a function that accepts a field name and returns an instruction that gets the respective field from a datatype.

Since field's existence and its type should be checked at compile-time, we would like the caller to provide field's name at the type level, not at the term level.
So, our method should look like:

```haskell
-- We will use Label from vinyl package
-- (<https://hackage.haskell.org/package/vinyl>).
-- Though it is easy to implement your own if extra dependencies
-- are undesired.
import Data.Vinyl.Derived (Label)

toField :: (...) => Label name -> (dt : s :-> GetFieldType dt name : s)
toField = undefined
```

Thanks to the [`OverloadedLabels`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-OverloadedLabels.html) extension, the `Label name` parameter can be initialized using `#myField` syntax, so the name of the field will be quite convenient to specify for the caller.

Now, what about the implementation?

When going with vanilla Generics, as it has been shown above, it is common to have a typeclass that traverses the generic representation of a type and thus incrementally builds the desired instruction.
But in cases like this, having a typeclass is not enough.
Here, we don't know in advance where to go over the generic tree to find the desired field; in this sense, _closed type families_ are more flexible because they can perform complex type-level computations.
Therefore, our getter macro is built in two steps:
Here, we don't know in advance where to go over the generic tree to find the desired field; in this sense, _closed type families_ are more flexible because they can perform complex type-level computations.
Therefore, building our getter macro is performed in two steps:

1. Using type families, construct a path (sequence of `L`s and `R`s) in generic representation to the desired field.
If the requested field is not found, return `TypeError`.

Previously, we intentionally gave an incomplete list of generic primitives; of course, they include field names and other info that we can use in our lookup.

2. Have a typeclass that descends over the datatype following the returned path and builds the corresponding sequence of `CAR` and `CDR` instructions.

After the first stage, we should get the following structure at the type level:
```haskell
-- | Result of field lookup — its type and path to it in the tree.
data LookupNamedResult = LNR Type Path

-- | Path to a leaf (field) in the generic tree representation.
type Path = [Branch]

-- | Which branch to choose in generic tree representation.
data Branch = L | R
```

Implementing field lookup takes a moderate amount of code. In case the reader is interested, it can be found below:

<details>
  <summary>Field lookup implementation</summary>

```haskell
-- Getters
type family LnrFieldType (lnr :: LookupNamedResult) where
  LnrFieldType ('LNR f _) = f
type family LnrBranch (lnr :: LookupNamedResult) :: Path where
  LnrBranch ('LNR _ p) = p

-- | Find a field of some product type by its name.
type GetNamed name a = LNRequireFound name a (GLookupNamed name (G.Rep a))

-- Lookup logic
type family GLookupNamed (name :: Symbol) (x :: Type -> Type)
          :: Maybe LookupNamedResult where
  GLookupNamed name (G.D1 _ x) = GLookupNamed name x
  GLookupNamed name (G.C1 _ x) = GLookupNamed name x

  GLookupNamed name (G.S1 ('G.MetaSel ('Just recName) _ _ _) (G.Rec0 a)) =
    If (name == recName)
      ('Just $ 'LNR a '[])
      'Nothing
  GLookupNamed name (G.S1 _ (G.Rec0 (NamedF f a fieldName))) =
    If (name == fieldName)
      ('Just $ 'LNR (NamedInner (NamedF f a fieldName)) '[])
      'Nothing
  GLookupNamed _ (G.S1 _ _) = 'Nothing

  GLookupNamed name (x :*: y) =
    LNMergeFound name (GLookupNamed name x) (GLookupNamed name y)
  GLookupNamed name (_ :+: _) = TypeError
    ('Text "Cannot seek for a field " ':<>: 'ShowType name ':<>:
     'Text " in sum type")
  GLookupNamed _ G.U1 = 'Nothing
  GLookupNamed _ G.V1 = TypeError
    ('Text "Cannot access fields of void-like type")

-- Helpers for merging results got in recursion
type family LNMergeFound
  (name :: Symbol)
  (f1 :: Maybe LookupNamedResult)
  (f2 :: Maybe LookupNamedResult)
    :: Maybe LookupNamedResult where
  LNMergeFound _ 'Nothing 'Nothing = 'Nothing
  LNMergeFound _ ('Just ('LNR a p)) 'Nothing = 'Just $ 'LNR a ('L ': p)
  LNMergeFound _ 'Nothing ('Just ('LNR a p)) = 'Just $ 'LNR a ('R ': p)
  LNMergeFound name ('Just _) ('Just _) = TypeError
    ('Text "Ambigous reference to datatype field: " ':<>: 'ShowType name)

-- Force result of 'GLookupNamed' to be 'Just'
type family LNRequireFound
  (name :: Symbol)
  (a :: Type)
  (mf :: Maybe LookupNamedResult)
    :: LookupNamedResult where
  LNRequireFound _ _ ('Just lnr) = lnr
  LNRequireFound name a 'Nothing = TypeError
    ('Text "Datatype " ':<>: 'ShowType a ':<>:
     'Text " has no field " ':<>: 'ShowType name)
```
</details>

After evaluating the exact location of the field, the necessary macro can be recursively constructed via the dedicated typeclass:

```haskell
-- | Generic traversal for constructing 'toField' macro for
-- a specific field.
class GIsoValue x =>
  GToField
    (name :: Symbol)
    (x :: Type -> Type)
    (path :: Path)
    (fieldTy :: Type) where

  -- | Gets a field from the given part of the datatype.
  -- Note that here we work at Michelson level, not at Lorentz,
  -- because we need access to the underlying tree-of-pairs
  -- representation.
  gToField
    :: GIsoValue x
    => Instr (GToT x ': s) (ToT fieldTy ': s)

-- | Skipping wrappers with meta info in generic representation.
instance GToField name x path f =>
         GToField name (G.M1 t i x) path f where
  gToField = gToField @name @x @path @f

-- | Recursion base.
instance (IsoValue f) =>
         GToField name (G.Rec0 f) '[] f where
  gToField = Nop

-- | Go-left case.
instance (GToField name x path f, GIsoValue y) =>
         GToField name (x :*: y) ('L ': path) f where
  gToField = CAR `Seq` gToField @name @x @path @f

-- | Go-right case.
instance (GToField name y path f, GIsoValue x) =>
         GToField name (x :*: y) ('R ': path) f where
  gToField = CDR `Seq` gToField @name @y @path @f

-- | Ready macro for accessing given field of the given datatype.
toField
  :: forall dt name s.
     (InstrGetFieldC dt name)
  => Label name -> (dt : s) :-> (GetFieldType dt name : s)
toField _ = I $
  gToField @name @(G.Rep dt) @(LnrBranch (GetNamed name dt))
           @(GetFieldType dt name)

-- | Constraint for `toField'.
type InstrGetFieldC dt name =
  ( Generic t, IsoValue t, ToT t ~ GValueType (G.Rep t)
  , GToField name (G.Rep dt)
      (LnrBranch (GetNamed name dt))
      (LnrFieldType (GetNamed name dt))
  )
```

Note that this time we do not provide instances for all Generics primitives — some cases are not possible by the construction of path to the field.
Rather, we mostly need to make sure that pattern match on a path is complete, that is, `[]`, `L : path'` and `R : path'` cases (plus one more with `M1` generic wrapper) are considered.


After all this work, a contract developer can write code in the following manner:

```haskell
someMethod = do
  stackType @[Storage]

  dup; toField #admin  -- no more car's and cdr's
  sender; assertEq ...

  dup; toField #paused
  not; assertEq ...
```

#### Other methods

All the Lorentz methods for working with datatypes include:

```haskell
-- * For product types
toField
getField      -- dup + toField
setField
modifyField
construct     -- makes up an object from scratch

-- * For sum types
wrap          -- wrap a value into constructor
unwrapUnsafe  -- unwrap value expecting the given constructor,
              -- fail otherwise
case
```

There is an unexpected snag when trying to make methods that work with sum types also accept labels.
Constructor names that we want to specify there always start with capital letter, however, labels [cannot start with them](https://gitlab.haskell.org/ghc/ghc/issues/11671).
We see two ways to work around this — either accept the constructor name via type application (`@"MyConstructor"`), or expect the constructor name prefixed with a lowercase letter.
Eventually, we went with the `c` (for constructor) prefix, like `#cMyConstructor`.

One last challenge was defining `case` syntactically and semantically sanely.
We could not use Haskell's `case` here, because it is way different from what we want.
At the end, we came up with the following syntax:

```haskell
myMethod =
  caseT
    ( #cConstructor1 /-> do
        stackType @(FieldOfConstructor1 : _)
        ...

    , #cConstructor2 /-> do
        ...
    )
```

Specifying the constructor name within `caseT` is not strictly necessary (could always be just `fromLabel`), we do so only to increase code legibility.
As `caseT` name suggests, this method accepts a tuple; with other possible interfaces, it is likely that the caller might need to enclose case clauses into parentheses which would be inconvenient.

### Drawbacks

The approach with Generics has several substantial drawbacks.

#### Error messages

At the current moment (we use GHC-8.8 at the moment of writing), it seems to not be possible to control the order in which constraints are checked.

With Generics, one often has to declare functions with constraints like:

```haskell
myFunc :: (Generic a, SomeConstraint (G.Rep a)) => ...
```

If the user forgets to declare the `Generic` instance for his datatype, then it is difficult to predict which constraint will fire — the first one, the second one, or even both.
And there's no easy way to influence this.

This is especially sad taking into account that the latter constraint may be big in result; when the `Generic` instance is not defined, the compiler cannot deduce `G.Rep a` and dumps the entire constraint it cannot reason about, which turns to be pretty confusing for users not well familiar with Haskell.
In our experience, even some developers spent an hour or more trying to understand the source of error, and after figuring it out just learned a rule "big error => any Generic instance forgotten?", which is indeed a sign of not the best UX ever.

One of the solutions to this is to turn stuck type family deduction into a type error as described in [this post](https://kcsongor.github.io/report-stuck-families/).
For example, the [`generic-lens`](https://hackage.haskell.org/package/generic-lens) library [uses](https://hackage.haskell.org/package/generic-lens-core-2.0.0.0/docs/Data-Generics-Internal-Errors.html) this approach to produce human-readable errors.
This still requires some extra effort and special care from the library developer's side.

#### Compilation time

Compilation of a contract with a large amount of datatypes and macros may take a decent amount of memory and CPU time, though in our experience, it can be kept within reasonable limits.

Let us first note that we build projects with the `-O0` flag, since the use of singletons in Michelson core already influences compilation time a lot.
That is not a big problem, because at runtime, optimization flags affect only speed of translation from Lorentz to Michelson which is fairly low.

The largest project we had used about 8Gb and 2-3 minutes to compile [^my-PC] with `-j4` or `-j0`, though the contract there contained a pretty lot of business logic (some of the endpoints barely fit into Tezos hard gas limit of 800k).
Moderately-sized contracts didn't consume any significant amount of resources.

[my-PC]: Intel(R) Core(TM) i7-7700HQ CPU @ 2.80GHz, SSD

#### Boilerplate

Implementing new functionality with vanilla `GHC.Generic` can be tedious as it involves writing a lot of instances and reusing logic with it is relatively difficult.
We went with it as it automatically provides balancing of sum and product types and requires little boilerplate on the user's side, though now when Lorentz has become quite big it might be worthy to switch to something else.

### Alternatives
#### Generics SOP

[generics-sop](https://hackage.haskell.org/package/generics-sop) package allows representing datatypes not as binary trees but as a list of lists of fields, and defines many handly utilities for working with such representation.
Maybe not entirely convenient for translating datatypes to Michelson (would require manual balancing), but can be handy for other Lorentz features which we will cover in upcoming posts.

#### Template Haskell

A completely different way would be using Template Haskell to analyze datatypes and generate the necessary type-level declarations (for Michelson representation), methods, and typeclass instances (for macros).
It may be slightly more difficult to work with since Haskell AST is vast, but having the generation logic at the term level allows for much better reusability and control over errors.

## Conclusion

In this post, we have considered the implementation of complex objects and respective macros in Lorentz.
Resulting functionality is similar to what other high-level languages over Michelson provide.
The overall implementation of objects can be found in the [Morley repository](https://gitlab.com/morley-framework/morley/-/tree/be72c7641d0fe368eff2b90ab0e21080ff1bc622/code/morley/src/Michelson/Typed/Haskell).
It also contains [several public contracts](https://gitlab.com/morley-framework/morley/-/tree/1722a7ab667a407ce4ed225bb1e5bce8434bfe77/morley-ledgers) using this feature.

In the following post, we will consider how Haskell's newtypes can be beneficial for ensuring the correctness of a Lorentz contract.

Stay with us!

ﾍ(=^･ω･^= )ﾉ

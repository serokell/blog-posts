# Lorentz: achieving correctness with types

[In the previous post](https://serokell.io/blog/lorentz-implementing-smart-contract-edsl-in-haskell), we introduced Lorentz base syntax which allows one to write code in the following manner:

```haskell
-- | Accepts distance on X and Y axes on stack
--   and pushes square of Euclidean distance back.
distance :: [Integer, Integer] :-> '[Natural]
distance = do
  dup; mul
  dip $ do dup; mul
  add; isNat
  if IsSome then nop
  else push [mt|Something went wrong|]; failWith
```

Michelson provides several basic primitives types and ways to group them in products (`pair`) and sums (`or`), and we still follow this behaviour.
As noted in a [spin-off post about Lorentz objects](https://serokell.io/blog/lorentz-complex-objects), the opportunity to group types into a sum of products corresponds to Haskell's datatypes, but one other feature remains unused.

In this post, we will talk about Haskell _newtypes_ and why they can be a powerful tool in a smart contracts eDSL like Lorentz.

<details>
  <summary>A note about type signature syntax.</summary>

In this article, we are going to write `[a, b] :-> [c, d]` in signatures, rather than `a : b : s :-> c : d : s`, for simplicity.
The reader may notice that such methods will not be general enough, but Lorentz has a `framed` function which makes any given instruction polymorphic over a stack tail, so that is not a big concern.

Also, by Haskell rules, lists of types with less than two elements have to be prefixed with a tick to avoid ambiguity with plain lists.

</details>

## Motivation

_Newtype_ is a type that is a mere wrapper over another already existing type but may have its custom semantics.
Newtypes are supposed to be free in terms of performance and only affect the type-checking stage, so they allow stricter correctness guarantees to be verified at compile time.

We have encountered two kinds of real-life examples where newtypes are useful:

1. Distinguishing different entities with the same representation.

For instance, it is common for advanced token smart contracts to allow participants to have multiple addresses with money.
The user probably wants to know the balances of those addresses, and eventually, we may need to have three methods for that:

```haskell
-- | Fetch balance of a single address.
getAddressBalance
  :: [Address, Storage] :-> '[Mutez]

-- | Evaluate the total balance of a participant.
getParticipantBalance
  :: [Address, Storage] :-> '[Mutez]

-- | Evaluate balance of the participant owning given address.
getOwnerBalance
  :: [Address, Storage] :-> '[Mutez]
```

When using these methods in the form they are mentioned, types don't help us at all.
It is possible to apply `getOwnerBalance` to an address of a participant by mistake.

If we manage to make bad code like this impossible to write, that should improve the development experience and correctness of the resulting contracts.

2. Types with invariants.

Let's imagine that business logic assumes you have several flags for each address: whether it is locked by the user, whether the user should be notified by e-mail on balance change, whether the user's cat is authorized to spend money from this address, \<insert your own\>.

A general efficient way to represent such a flag set is by keeping a numeric bitmask.
Following the fail-fast principle, you want to ensure that an invalid bitmask is not produced at any stage across the contract.
And, actually, there might be many places whether the bitmask can become invalid – contract origination, separate flag changes (in case some flags can contradict with others), and changes as a whole (batch update by user). You would like to make sure that no validity check is missing in any of these places.

In general Haskell code, such problems are often solved by declaring a newtype with explicitly stated invariants and providing constructors/modifiers which treat those invariants.

## Our approach

In Lorentz, we declare newtypes as follows:

```haskell
newtype MyType = MyType Integer
  deriving newtype IsoValue
```

Let us remind that `IsoValue` describes how Lorentz types are translated into Michelson, and the `deriving newtype` clause is Haskell's syntax that says that behaviour of `IsoValue` should be inherited from the inner type, `Integer` in our case.

Now one can write:

```haskell
putDefaultMyType :: '[] :-> '[MyType]
putDefaultMyType = push (MyType 0)
```

That was simple.

The main benefit we get is that `MyType` is treated as a type completely different from `Integer` or any other newtype wrapper over `Integer`, so misapplying a function to a wrong argument would raise a compilation error.

Now we would like to have dedicated methods for converting between newtype and its inner representation.
Lorentz already defines `forcedCoerce_` method that converts between any two types having the same Michelson representation, but it is pretty broad and requires special care from the developer to be used correctly: a typo or a wrong mental model is pretty likely to cause a bug, so this method should be avoided in business logic layer.

We would like to allow users to be specific in their intentions via methods specialized to newtype wrapping/unwrapping. For this reason, we define the following methods:

```haskell
coerceWrap :: Wrappable a => '[Unwrappable a] :-> '[a]
coerceWrap = forcedCoerce_

coerceUnwrap :: Wrappable a => '[a] :-> '[Unwrappable a]
coerceUnwrap = forcedCoerce_
```

<details>
  <summary>One more note about type signatures.</summary>

Being part of the library, these two methods are of course defined in a [more generic way](https://gitlab.com/morley-framework/morley/-/blob/99fbc99defdedd02568f59cc692c0f021cc9209e/code/lorentz/src/Lorentz/Coercions.hs#L90)

</details>

Note the `Wrappable a`: when our newtype does not have any invariants and can be wrapped and unwrapped safely, we declare it to have a [`Wrappable`](https://gitlab.com/morley-framework/morley/-/blob/0574c85b78214129b5673b0f8e09b0761711522f/code/lorentz/src/Lorentz/Wrappable.hs#L20) instance.
It looks like this:

```haskell
newtype Participant = Participant Address
  deriving stock (Generic)
  deriving anyclass (IsoValue, Wrappable)

authenticate :: '[Participant] :-> '[]
authenticate = do
  coerceUnwrap  -- unwraps Participant to Address
  sender        -- pushes address of the current transaction executor
  assertEq [mt|Method executed not by the expected participant|]
```

We deem `coerceWrap` to be always safe: if a given newtype has invariants, it should not instantiate `Wrappable` typeclass in favor of another way we will consider a bit later.

### Newtypes with invariants

Now let us consider the previously mentioned example with bitmasks.
As before, we declare a dedicated newtype:

```haskell
-- | Invariant: keeps a 3-bits number.
-- Use the dedicated smart constructor to make a value of this type.
newtype Flags = FlagsUnsafe { unFlags :: Natural }
  deriving newtype IsoValue
```

And there were three use cases for it which we wanted to support.
The first one is constructing a Haskell value that can later be provided in contract origination.
This can be done safely in various ways using built-in Haskell features and is up to the developer.

<details>
  <summary>We would implement it as follows.</summary>

```haskell
-- | Constants, building blocks for 'Flags'.
flag1, flag2, flag3 :: Flags
flag1 = FlagsUnsafe 1
flag2 = FlagsUnsafe 2
flag3 = FlagsUnsafe 4

-- | Provides union of two flags sets.
instance Semigroup Flags where
  a <> b = Flags (unFlags a .|. unFlags b)

-- | Provides empty flags set.
instance Monoid Flags where
  mempty = FlagsUnsafe 0

-- Here also getting "mconcat :: [Flags] -> Flags" with
-- semantics of the union operation

instance Bounded Flags where
  minBound = FlagsUnsafe 0
  maxBound = FlagsUnsafe 7

-- | An arbitrary example.
myCustomFlags :: Flags
myCustomFlags = mconcat [flag1, flag2]

-- As it can be seen, constructing an invalid Flag value
-- (with the inner number greater than 7) is not possible without
-- using unsafe methods.
```

</details>

Next, we need to make sure that contract code never produces an invalid value when operating with `Flags`.
Michelson has many polymorphic operations (`ADD`, `NOT`, `OR`), and it so happened that in Lorentz, one has to explicitly declare which of these operations are permitted for the introduced types.
In our case, this is actually handy: if we allow only logical operations on our `Flags` type, then its invariants will never get violated during contract execution.

```haskell
-- | Flags intersection.
instance ArithOp And Flags Flags where
  type ArithRes And Flags Flags = Flags

-- | Flags union.
instance ArithOp Or Flags Flags where
  type ArithRes Or Flags Flags = Flags

-- Defining e.g. 'Not' operation does not make much sense for our case,
-- so we do not permit this.
```

In case some flags can contradict  each other, we would rather define a special function that adds flags with care:

```haskell
-- | Add 'flag1', fail if it is not possible concerning other present flags.
addFlag1 :: '[Flags] :-> '[Flags]
addFlag1 = do
  ... -- check there are no contradicting flags
  push flag1
  uniteFlagsUnsafe

uniteFlagsUnsafe :: [Flags, Flags] :-> '[Flags]
uniteFlagsUnsafe = do
  forcedCoerce_ @Natural; dip (forcedCoerce_ @Natural);
  or
  forcedCoerce_
```

Using this set of coercions may seem to make the contract less efficient, but because of the optimizer (which any eDSL should have for various reasons), these `forcedCoerce_` calls will not appear in the resulting Michelson code.

Finally, the last case is accepting a user-provided value.
This can be useful when the user wants to overwrite the existing value of a flag, and we need to make sure that his input is indeed valid.
Here, newtype helps us draw a line between checked data we can work with or store (`Flags`) and yet unchecked data we accept as input (let it be just `Natural` for simplicity).

```haskell
-- | When we know for sure that provided value is valid...
toFlagsUnsafe :: '[Natural] :-> '[Flags]
toFlagsUnsafe = forcedCoerce_

-- | ...and when we don't.
toFlags :: '[Natural] :-> '[Flags]
toFlags = do
  dup
  push (maxBound @Flags); forcedCoerce_ @Natural
  if IsLe
  then do push [mt|Invalid flags value|]; pair; failWith @(MText, Natural)
  else toFlagsUnsafe
```

The bottom line: in Lorentz, one can use newtypes in a very similar way to how it usually happens in general Haskell code.
Some wrap/unwrap boilerplate in the form of `forcedCoerce_` calls will still be involved, but now the type system will assist you in ensuring that all the data is used correctly.

## Common questions

### Do I need types if I can write tests?

While types are not meant to fully replace tests, a single newtype wrapper can eliminate the need for an entire category of tests just because buggy code cannot be written using safe primitives anymore.
Furthermore, the type system hints the developer if some check is missing.
This is an especially useful property for library writers, as their users also get a piece of that safety automatically.

Also (this point is often not taken into account) tests may form quite a large codebase that also has to be maintained, so you would like to avoid writing repetitive tests for trivial failure cases where possible.

For more details, I recommend Ken Fox's [article](https://spin.atomicobject.com/2014/12/09/typed-language-tdd-part1/) explaining why it is beneficial to have both types and tests.

### What about formal verification?

Unlike the described newtypes approach, formal verification allows you to express arbitrary properties on your data and thus has a significantly broader scope.

However, in practice its use is pretty complicated:

1. You may need to know a special proof assistant language like Coq to verify your programs;
2. You have to write proofs yourself.

This way, formal verification may often be infeasible for large contracts.

On the other hand, newtypes feature lets you put a constraint only on one piece of data at a time, so, for instance, `amount > 0 || storage.x > 0` invariant cannot be expressed here; but this feature is naturally embedded in the language and requires a constant amount of boilerplate.

## Conclusion

In this post, we have considered how Haskell's newtypes feature can be used to distinguish semantically different types, express guarantees at type-level, and ensure validation.

The mentioned functionality has been successfully used in several production contracts.
Its implementation can be found in the Morley repository ([one](https://gitlab.com/morley-framework/morley/-/blob/99466a49468be9374b5b454b844df9abe519edf8/code/lorentz/src/Lorentz/Wrappable.hs), [two](https://gitlab.com/morley-framework/morley/-/blob/99466a49468be9374b5b454b844df9abe519edf8/code/lorentz/src/Lorentz/Coercions.hs)).

Stay with us!

ﾍ(=^･ω･^= )ﾉ

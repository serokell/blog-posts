# Lorentz: implementing smart contract eDSL in Haskell

This is the first post in a series about Lorentz — Haskell eDSL for [Michelson](https://gitlab.com/camlcase-dev/michelson-tutorial/blob/master/README.md) smart contracts language.
In it, we look at how uncommon Haskell features can be used in real life.
The post also might be interesting for people working with smart contracts on Tezos or elsewhere to see how contract language supported by built-in features of a general-purpose language may look like.

[One of the previous posts](https://serokell.io/blog/parsing-typed-edsl) describes how we have reimplemented Michelson core into Haskell, which gave us an opportunity to parse textual contracts, interpret them and eventually cover them with tests.
However, when it came to the need to write our own contracts, it became clear that writing production Michelson code manually is in many senses inconvenient, so we along with [camlCase](https://twitter.com/camlcasetech) initiated work on Lorentz under [Tocqueville Group](https://serokell.io/blog/parsing-typed-edsl)'s management.

In this post, we are going to describe the very basic features of Lorentz.
Bringing a general-purpose language such as Haskell into Michelson contract development provides extremely broad opportunities that cannot be covered just within a single post, so features like objects, optimizations, contract documentation, type-safe upgradeable types and migrations will be covered in sequels.

### Why Haskell?

Haskell is a language with a pretty powerful type system, it fits quite well for writing eDSLs.

On the one hand, languages with custom syntax and feature set that have their own translator cannot be easily extended by a user when necessary; new features usually appear slowly and may require community approval.
In this sense, using one of the existing general-purpose languages allows users to write arbitrary extensions without modifying the base engine; it enables simpler prototyping because one doesn't need to write a parser and (ideally) type system drives the user through the process.

On the other hand, general-purpose dynamically typed languages (like Python), as well as most strongly typed languages (like Java) lack expressive power to provide a user with a fully safe interface.
For instance, imagine a macro which takes an object and returns one of its fields by name, will the language ensure that type of returned field is a valid one, or that this field even exists?
At runtime — for sure, at compile time — hardly.
One can say that we may provide a sort of static analyzer for that, but then we run into all the same problems of special languages.

## Lorentz base

[The previously mentioned post](https://serokell.io/blog/parsing-typed-edsl) introduces primitives used for reimplementing the Michelson engine in Haskell.
Generally, we could write contract code in Haskell using those primitives as they are, but that would be a thankless job.
In this section, we are going to describe a way to hide some implementation details from a contract developer thus providing a more natural Haskell API for writing Michelson code.

### Values

Lorentz starts from the idea that contract writers should operate with Haskell values, and underneath they will be turned into their Michelson counterparts.

Previously, we introduced datatypes representing Michelson types and values, let us recall how they looked like (with minor changes applied):

```haskell
data T =
   TInt
 | TNat
 | TOption T
 | TList T
 | TPair T T
 -- There is more in actual Michelson

data Val t where
  VInt :: Integer -> Val 'TInt
  VNat :: Natural -> Val 'TNat
  VOption :: Maybe a -> Val ('TOption a)
  VList :: [Val t] -> Val ('TList t)
  VPair :: Val p -> Val q -> Val ('TPair p q)
```

Apparently, we don't want to pass `VInt 5` or `VPair a b` around, rather it should be possible to use bare `5` and `(a, b)`.
So we begin with defining an isomorphism between Haskell and Michelson values for each possible type.

```haskell
class IsoValue a where
  type ToT a :: T
  toVal :: a -> Val (ToT a)
  fromVal :: Val (ToT a) -> a

instance IsoValue Integer where
  type ToT Integer = 'TInt
  toVal = VInt
  fromVal (VInt x) = x

instance (IsoValue a, IsoValue b) => IsoValue (a, b) where
  type ToT (a, b) = 'TPair (ToT a) (ToT b)
  toVal (a, b) = VPair (toVal a) (toVal b)
  fromVal (VPair a b) = (fromVal a, fromVal b)

...
```

Now one can write:

```haskell
λ> toVal (1 :: Integer, 2 :: Natural)
VPair (VInt 1) (VNat 2)
```

Actually, there are two different approaches we could take here.

The set of Michelson types is closed, meaning that the user cannot add his own types to it.
We could make it the same in Haskell, then the compiler would always be able to deduce the Haskell type by the Michelson type derived from it (thanks GHC for `TypeFamilyDependencies` extension), this would have its benefits.

In contrast, we went about it differently, allowing multiple Haskell types to have the same Michelson representation so that the user could define custom types built from basic primitives.
This does not increase expressive power but provides substantial assistance of type system similar to how it happens in a general Haskell code when one defines his custom newtype wrappers and datatypes, as well as opens a venue for other programmers to write plugins to the base engine.

### Instructions

Let us remind that in Michelson one writes code as a sequence of instructions which operate on a typed stack, and the set of allowed instructions is defined as follows:

```haskell
data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Instr a b -> Instr b c -> Instr a c
  Nop :: Instr s s

  PUSH :: Val t -> Instr s (t : s)
  PAIR :: Instr (a : b : s) ('TPair a b : s)
  ...
```

Taking into account our `IsoValue` typeclass, now we can write code like

```haskell
withZero :: Instr (ToT Integer : s) (ToT (Natural, Integer) : s)
withZero = PUSH (toVal (0 :: Natural)) # PAIR

-- Helper operator
(#) :: Instr a b -> Instr b c -> Instr a c
(#) = Seq
```

Constantly calling all these `ToT`s and `toVal`s is apparently not what we want, and it would be nice to hide this logic within appropriate aliases.

But before starting, let's make a single observation.
Previously, we assumed that multiple Haskell types should be able to turn into the same Michelson type, so `ToT` is not an injective type family.
This means that when one writes `ToT Integer`, information about the original `Integer` type gets immediately forgotten and type checker only knows about the resulting `TInt`.
Thus, for instance, if we remove `:: Natural` part in the code snippet above, GHC won't be able to guess from the function signature that `0` is of type `Natural` — it only sees that type of `toVal 0` is `ToT Natural` and that is too ambiguous.

How do we fix the original types?
We declare a datatype!

Let's illustrate this for a simplified case.
Say we have a datatype declared as follows:

```haskell
newtype MyType a = MyType Int
```

It is a well-known fact that `MyType a` and `MyType b` are treated as different types despite having the same representation (and in such case `MyType` is said to have a _phantom_ type parameter):

```haskell
convert :: MyType () -> MyType Int

-- This does not work.
convert a = a

-- We have to recreate our type from scratch.
convert (MyType a) = MyType a
```

So datatypes (and newtypes) fix their type arguments up to the moment when they are deconstructed.
The same technique can be used for fixing Haskell types lying on stack.
We declare a newtype wrapper over `Instr` type and then mirror all Michelson instructions in the new representation.

```haskell
infix 1 :->
newtype (inp :: [Type]) :-> (out :: [Type]) =
  I { unI :: Instr (ToTs inp) (ToTs out) }

type family ToTs (ts :: [Type]) :: [T] where
  ToTs '[] = '[]
  ToTs (x ': xs) = ToT x ': ToTs xs

-- * Mirrored instructions

(#) :: (a :-> b) -> (b :-> c) -> (a :-> c)
I l # I r = I (Seq l r)

push :: IsoValue t => t -> (s :-> t : s)
push = I $ PUSH (toVal a)

pair :: a : b : s :-> (a, b) : s
pair = I PAIR
```

Now we can smoothly write contract code as if Michelson was naturally designed in Haskell:

```haskell
withZero :: Integer : s :-> (Natural, Integer) : s
withZero = push 0 # pair
```

What we effectively got after all this work:

1. Our initial assumption about the open set of types allows defining custom newtypes and datatypes on top of the fixed set of Michelson types. We will come back to this ~later~ someday.

2. It is not possible to apply an instruction accepting a stack of type `[x]` to a stack of type `[y]` even if `x` and `y` have the same Michelson representation. I.e. our type-system does not allow implicit casts, which seems good and quite Haskell-ish.

3. Error messages prompted by the compiler on type mismatches will now also mention Haskell types, not the Michelson ones — the latter would indeed be quite confusing for contract developers.

#### Coercions

For the rare cases when developer knows what they are doing, we should supply a function for explicit casts.

```haskell
type MichelsonCoercible a b = ToT a ~ ToT b

coerce_ :: MichelsonCoercible a b => (a : s) :-> (b : s)
coerce_ =
  -- under 'I' constructor Haskell types are not fixed, so this works
  I Nop
```

That's it.

Note that these `Nop`s are eliminated upon translating code to Michelson.
As our practice shows, such eDSL should have at least a primitive optimizer for various cases anyway, otherwise, the user would constantly have to choose between writing clear code with macros, reusable functions e.t.c and micro optimizations.

<details>
  <summary>Some random thoughts about naming</summary>

Actually, our `coerce_` method semantically takes the middle-ground between Haskell's `coerce` and `unsafeCoerce`, so this naming is a bit unfortunate.

It is not exactly `coerce`, because `Coercible` constraint in Haskell tends to preserve type invariants.
A known example is `Set a` — one cannot simply call `coerce :: Set a -> Set (MyNewtype a)` because that may break inner `Set` structure in case `MyNewtype` modifies `Ord` instance.
Also, if some newtype has invariants which may break on its type argument change, and coercions are used in code extensively, it seems like a good practice to assign type roles to type arguments (in order to prohibit undesired arguments changes via coercion) and to hide newtype constructor (to avoid wrap-unwrap coercions) when necessary.
This way we can sleep well knowing that `coerce`s scattered across the code never cause a bug.

So unlike Haskell's `coerce`, our `coerce_` is less safe because it can violate type invariants.
On the other hand, it is not as unsafe as `unsafeCoerce` because it never segfaults nor produces invalid contracts.

Eventually, we stopped at `forcedCoerce_` name for that function.

A proper way to implement safe `coerce_` is yet under discussion.
An obvious solution would be to additionally require Haskell's `Coercible` in it, but (taking the specificity of Lorentz use cases into account) this solution has its own substantial drawbacks.

We will return to this topic in one of our next posts.

</details>

## Syntax

Haskell syntax is very flexible and concise, it fits pretty nice for writing eDSLs.
Further, we consider how its features allow reaching the same quality of syntax as in many non-embedded DSLs (like Michelson itself) and even better.

### Type applications

Michelson supposes the use of a simple typechecker where type information goes strictly from the beginning of code to its end.
This means, that while some instructions like `CAR :: (a, b) : s -> a : s` can deduce type of result by themselves, other instructions like `PUSH t (v :: t) :: s -> t : s` or `LEFT b :: a : s -> or a b : s` always require specifying type explicitly.

Not so in Haskell. GHC typechecker performs generic unification thus being able to deduce type information whenever it can be "guessed". For instance:

```haskell
code1 :: s :-> Integer : s
code1 = push 5
-- ^ Type of pushed constant can be derived from expected result type

code2 :: IsoValue b => a : s :-> Either a b : s
code2 = left
-- ^ And same here, we only require `IsoValue b` as a proof of that
-- `b` turns into an actual Michelson type
```

A common case when we still have to prompt types to typechecker via the dedicated `TypeApplications` extension is pushing numbers: Michelson has separate `int` and `nat` types, and in code like `push 1 # push 2 # sub` it's not clear which types are meant because both numeric literals and `sub` instruction are polymorphic.
In such cases, Haskell allows us to pass optional annotations which clarify types, for instance, `push` instruction allows specifying a type of value to be put:

```haskell
push @Integer 1 #
push @Natural 2 #
sub
```

Use cases for type clarifications are not limited to pushing numbers, they can be used to ensure readability.
For instance,

```haskell
drop @()
```

specifies developer's intentions way clearer than simple

```haskell
drop
```

and this may be a boon when delving into large blocks of code.

Also, we can declare an instruction which fixes the current stack:

```haskell
stackType :: s :-> s
stackType = nop  -- that was trivial

code =
  ...
  stackType @(Integer : Integer : _) #
  add #
  applyCoefficients #
  stackType @(Natural : _) #
  ...
```

Aside from serving as a hint for a code reader (profit of that for real-world contracts cannot be overestimated), this significantly helps in locating typecheck errors when code is being modified.

### Code blocks

Previously we introduced `#` operator as a glue between instructions, but using it constantly is annoying.

Fortunately, most of Haskell's sugar can be overloaded to work with user functions via `RebindableSyntax` extension.
In our case, we would like to exploit `do`-blocks syntax.

Apparently, our instruction type `:->` cannot be an instance of `Monad`, so by default, it is not possible to make instruction a direct part of the `do`-block.
To deal with this, we define our custom `>>` operator.

```haskell
(>>) :: (a :-> b) -> (b :-> c) -> (a :-> c)
(>>) = (#)
```

Now one can write

```haskell
{-# LANGUAGE RebindableSyntax #-}

code = do
  push @Integer 5; dup
  dip $ do
    swap
    sub
    ...
  ...
```

What about `return` and `>>=` from the `Monad` typeclass?
The former is not necessary for `do`-blocks to work; the latter is used for arrow syntax (`a <- instr`), and in the given stack machine language we didn't manage to find a practical use case for it.

A bit of polishing at the last:
* If `-XApplicativeDo` extension is enabled, `do`-blocks tend to use `fmap`, `pure` and `<*>` functions whenever it is possible not to fall back to `>>=` and `>>`. We definitely cannot implement those sanely for our instruction type, so this extension has to be disabled.
* Compiler thinks that each instruction, being a standalone part of `do`-block, "returns" something but its "result" as of "action" is not used. Thus `-Wunused-do-bind` warning should be disabled as well.

There might be an unexpected profit from `-Wunused-do-binds` though: it's a common wish to be able to know the current stack at a given instruction, and given extension allows viewing those as a set of warnings. Works especially good with IDE showing errors/warnings at the cursor position.

### Conditionals

In Michelson, `IF`-like instructions accept clauses as operands and pick predicate from the stack.
For instance:

```
PUSH bool True;
IF { /* Do one thing */ } { /* Do another thing */ };
```

or

```
PUSH (or int nat) (Left 5);
IF_LEFT { /* Action for Left case */ } { /* Action for Right case */ };
```

This differs from most high-level languages where `if` accepts condition as expression.

In Haskell, it's possible to redefine the semantics of `if ... then ... else ...` construction via declaring alternative `ifThenElse` function, though it still should accept 3 operands.
To fit in our case, we implement `if` as something intermediate between common `if` and conditional jumps from assembly language.
As the first operand, we will accept the type of condition — it may be checking a boolean value, comparing two values on stack or pattern-matching on `or` value.
Since the type of stack accepted by `if` clauses will depend on the condition type, we define this condition type using GADTs.

```haskell
data Condition arg argl argr where
  Holds :: Condition (Bool : s) s s
  IsNone :: Condition (Maybe a : s) s (a : s)
  IsLeft :: Condition (Either l r : s) (l : s) (r : s)
  IsZero :: ArithOpApplies Eq' a => Condition (a : s) s s
  IsEq :: ArithOpApplies Eq' a => Condition (a : a : s) s s
  ...

ifThenElse
  :: Condition arg argl argr
  -> (argl :-> o) -> (argr :-> o) -> (arg :-> o)
ifThenElse = \case
  Holds -> if_
  IsNone -> ifNone
  IsLeft -> ifLeft
  IsZero -> \l r -> eq # if_ l r
  IsEq -> \l r -> compare # eq # if_ l r
  ...
```

It would also be possible to define an open set of conditions via typeclass instead of GADT if necessary.

Summing up, at this moment we can write the following code:

```haskell
push @Integer 5; push @Integer 3
if IsEq
then do
  -- Action
else do
  -- Another action
```

### Named variables

When working with a flat stack of unnamed variables, it sometimes gets difficult to understand what currently lies on it and in which order.

We resolve this problem by (ab)using the [`named`](http://hackage.haskell.org/package/named) package.
In particular, we make use of its primitives for attaching names to types, it looks like `"var" :! Integer`.
The code which provides support for them on Lorentz field is limited to:

```haskell
import Data.Vinyl.Derived (Label)

instance IsoValue (name :! a) where
  type ToT (name :! a) = ToT a
  ...

-- | Attach a label to the variable at the stack top.
toNamed :: Label name -> (a : s :-> (name :! a) : s)
toNamed _ = coerce_

-- | Remove a label from the variable at the stack top,
-- expecting it to have the given name.
fromNamed :: Label name -> ((name :! a) : s :-> a : s)
fromNamed _ = coerce_
```

And now one can write:

```haskell
withdraw = do
  ... -- get balance
  toNamed #balance
  ... -- get withdrawn amount
  toNamed #withdrawal
  swap
  substractTokens
  ...

substractTokens :: ["balance" :! Natural, "withdrawal" :! Natural] :-> [Natural]
```

Here we used `#` syntax coming from the [`OverloadedLabels`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-OverloadedLabels.html) extension which allows fixing a type-level string with a term-level literal (though its use cases are broader than that).

We don't provide any automatic ordering of stack variables when calling a function because Lorentz is a low-level language.
Rather these names are a safety measure and developer is still responsible for proper stack management like calling `swap` in the example above (or contrary, he may want to swap the whole computations for the sake of more optimal code).

The next observation here is that we have several arithmetic and comparison instructions that accept two entities of the same type and for which order matters.
Visually ensuring that each such instruction is used properly is tiresome, and doing that again on each refactor is tiresome doubly.
It would be nice to check via the type system that upon calling those instructions we have the expected variables on the stack.

```haskell
checkHasEnoughTokens :: [Natural, Storage] :-> [Storage]
checkHasEnoughTokens = do
  dip getCurrentAmount
  stackType @[Natural, Natural, Storage]
  if ???  -- IsGe or IsLe?
  then nop
  else fail
```

We can make sure that upon calling `if` block variables are named, but that is still inconvenient when `if` itself accepts unnamed variables.
Such unpleasantness can be fixed neatly by adding appropriate condition types:

```haskell
data Condition arg argl argr where
  ...
  IsLeNamed
    :: Comparable a
    => Label n1 -> Label n2
    -> Condition ((n1 :! a) ': (n2 :! a) ': s) s s

  IsGeNamed
    :: ...

-- * Aliases

(<=.) = IsLeNamed
(>=.) = IsGeNamed
```

So now the example above can look like this:

```haskell
checkHasEnoughTokens :: [Natural, Storage] :-> [Storage]
checkHasEnoughTokens = do
  toNamed #withdrawn
  dip $ do getCurrentAmount; toNamed #balance

  stackType @[_ :! Natural, _ :! Natural, Storage]

  if #withdrawn <=. #balance
  then nop
  else fail
```

Now misordering stack operands will lead to a type error.
To get a clue on how this can help at production scale, you could imagine a dozen instructions inserted between `toNamed` calls and `if`, along with one more `Natural` present on the stack from the beginning.

It sounds like a good thing to similarly update all other instructions like `sub` and `div`.
On the other hand, this solution can be considered as only partial because the resulting boilerplate has its costs.

### Safe values construction

We are nearing the end of this post, and in the last section let's consider the fact that some of the Michelson types have an expectedly limited definition domain.
Values of types `nat` or `mutez` (the underlying currency) cannot be negative, `string`s allow only some ASCII characters.
Once we are used to writing type-safe code, it's frustrating to find out that we cannot safely construct values.

```haskell
-- something went wrong here
push @Natural (-1)

-- such value is also not valid
push @Text "A text\0"
```

Let's deal with each of the mentioned types in turn.

For `Natural` case, GHC prompts a warning saying that `-1` literal is not valid for this type, so everything is fine here.
But at the moment of writing this post, such checks are [hardcoded in GHC](http://hackage.haskell.org/package/ghc-lib-0.20190423/docs/src/MatchLit.html#warnAboutOverflowedLiterals) and apply only to some primitive types, so replicating this behaviour on `Mutez` does not seem feasible.

We workarounded it as follows: `Word` has a strictly smaller definition domain than `Mutez`, and at the same time it is one of those types for which GHC can prompt the mentioned warning.
So we added a function `toMutez :: Word -> Mutez`, and now one can safely write

```haskell
push (toMutez <num literal>)
```

which in most cases is enough.

Approaching string literals requires a different solution.
`IsString` typeclass allows performing only runtime checks, so it does not suit our purposes.
Type-level analysis of `Symbol` also seems complicated at the current stage of GHC existence.
Eventually, we went with `QuasiQuotes`, they exactly allow performing arbitrary parsing at compile time.
Definiing a custom quasi quoter is as simple as

```haskell
import qualified Language.Haskell.TH.Quote as TH

-- @mt@ for "Michelson text"
mt :: TH.QuasiQuoter
mt = TH.QuasiQuoter
  { TH.quoteExp = \s ->
      case parseQuasiQuoterText s of
        Left err -> fail $ toString err
        Right txt -> [e| toText @String txt |]
  , TH.quotePat = \_ -> fail "Cannot use at pattern position"
  , TH.quoteType = \_ -> fail "Cannot use at type position"
  , TH.quoteDec = \_ -> fail "Cannot use at declaration position"
  }
```

The unpleasant thing with quoters is that some find them cumbersome to type and, sometimes, read:

```haskell
push [mt|Some text here|] # push [mt|... and some here|]
```

Though, indeed, this depends on the amount of support (syntax highlighting and snippets) a particular IDE provides to a developer.

One more problem is that a quite wide-spread but outdated `haskell-src-exts` library fails to read the entire module if this module has uses of quasi quotes, thus they can interfere with existing code management workflows. Quoting the reaction of @gromak:

> Apparently, this `mt` thing exists only to circumvent checks performed by hlint.

So in cases when correctness is likely and can be easily verified by tests, it might be worth thinking carefully before introducing compile-time checks this way.

## Conclusion

In this post, we have described the foundation of Lorentz, including common syntax, primitives and funny Haskell constructions involved.
With the addition of some other features like objects, it has been successfully used for developing several production contracts and the accompanying tooling.
Some public contracts written on Lorentz can be found in the [Morley repository](https://gitlab.com/morley-framework/morley/-/tree/1722a7ab667a407ce4ed225bb1e5bce8434bfe77/morley-ledgers).

As one can guess, writing real-world contracts on a stack-machine language is a pleasure below average, and now we are working on fleshing out once prototyped high-level language over Lorentz called Indigo.
In the next posts, we are going to describe the essence of Indigo and touch advanced features used both in Lorentz and Indigo.

Stay with us

ﾍ(=^･ω･^= )ﾉ

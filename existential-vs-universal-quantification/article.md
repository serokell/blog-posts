# Universal and Existential Quantification in Haskell

In logic, there are two common quantifiers: the universal quantifier and the existential quantifier. You might recognize them as $\forall$ (for all) and $\exists$ (there exists).

They are relevant to Haskellers as well, since both universal and existential quantification are possible in Haskell.

In this article, we'll cover both types of quantification.

You'll learn how to:

* Make universal quantification explicit with  `ExplicitForAll`.
* Create a heterogeneous list with existential data types.
* Use existentially quantified type variables to make instantiation happpen at the definition site.

## Universal quantification

In Haskell, all type variables are universally quantified by default. It just happens implicitly.

As an example, look at the `id` function.

```haskell
id :: a -> a
```

We can think of it as "for all types $a$, this function takes a value of that type and returns a value of the same type".

With the [`ExplicitForAll`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html) extension, we can write that down explicitly.

```haskell
{-# LANGUAGE ExplicitForAll #-}

id :: forall a. a -> a
```

The syntax is simple – at the beginning of a function or instance declaration, before any constraints or arguments are used, we use
the `forall` quantifier to introduce all type variables that we'll use later.

Here, we introduce four variables: `a`, `b`, `c`, and `m`:

```haskell
func :: forall a b c m. a -> b -> m c -- OK, it compiles
```

All type variables that you introduce in your type signature should belong to exactly one `forall`. No more, no less.

```haskell
func :: forall a b c.   a -> b -> m c -- Error, type variable `m` is not introduced
```

Of course, we can also add constraints.
For example, we might want to specify that the `m` variable needs an instance of the `Monad` typeclass.

```haskell
func :: forall a b c m. Monad m => a -> b -> m c
```
So far, it might not seem very useful.
Nothing changes when we add the quantifier because it's already there (although implicit).
On its own, `ExplicitForAll` doesn't do a lot.

However, making the quantification explicit allows us to do some new things.
You frequently need it to work with other extensions.
Some of them will work better, and some of them just won't work at all without the `forall` quantifier. :)

### Practical use cases of universal quantification

#### Reordering type variables

With `ExplicitForAll`, you can change the order that type variables appear in the `forall` quantifier.

Why is that useful?
Let's imagine you want to define a function with 10 type variables. 

```haskell
veryLongFunction :: a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...
```

(Yes, there are [real-life examples](https://hackage.haskell.org/package/leancheck-0.9.10/docs/Test-LeanCheck-Utils-TypeBinding.html#v:-45--62--62--62--62--62--62--62--62--62--62--62--62-:) of such functions.)

And when you use it, you want to instantiate the last type variable explicitly.

---
**Instantiation** 

Before we proceed, a quick note on the _instantiation_ of type variables – the process of _plugging in_ a type to replace a type variable.

You can either let GHC do instantiation implicitly:

```hs
fst :: forall a b. (a, b) -> a
fst (x, _) = x

pair :: (Int, String)
pair = (1, "a")

-- The argument of `fst` is of type `(Int, String)`,
-- so GHC infers how to instantiate its type variables:
-- a=Int, b=String.
x = fst pair
```

Or you can do it explicitly with the [`TypeApplications`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html) extension:

```hs
{-# LANGUAGE TypeApplications #-}

-- Explicitly instantiate fst's first type variable to Int
-- and the second type variable to String.
x = fst @Int @String pair
```

`TypeApplications` assigns types to type variables in the order they appear in the (implicit or explicit) `forall`.

Explicit instantiation with `TypeApplications` is often used to give GHC a hand when it's having trouble inferring a type or simply to make code more readable.

---

Now, without an explicit `forall`, you would write something like this:

```haskell
veryLongFunction :: a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @_ @_ @_ @_ @_ @_ @_ @_ @_ @Integer ...
```

Quite long, right?
However, this can be simplified with an explicit `forall`:

```haskell
veryLongFunction :: forall j a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @Integer ...
```

Since `j` is the first type variable in the declaration of `veryLongFunction`, you can explicitly instantiate only `j` and omit the others.

### Supporting `ScopedTypeVariables`

A common extension that needs `ExplicitForAll` is [`ScopedTypeVariables`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/scoped_type_variables.html#).

The code below may seem reasonable, but it will not compile.

```haskell
example :: a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: [a]
    pair = [x, x]
```

It seems reasonable because it _looks_ like both functions are referring to the same type variable `a`.
However, GHC is actually inserting an implicit `forall` in both functions.
In other words, each function has its own type variable `a`.

```hs
example :: forall a. a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: forall a. [a]
    pair = [x, x]
```

We can rename one of those type variables to make the issue even more obvious:

```hs
example :: forall a. a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: forall b. [b]
    pair = [x, x]
```

Now it's clear that `pair` is a polymorphic function that promises to return a list of any type `b`,
but its implementation actually returns a list of type `a`.

What we _meant_ to say was that `pair` should be a monomorphic function that return a list of the type `a` declared in `example`.

To fix this, we can enable the `ScopedTypeVariables` extension.
With this, the type variables declared in an explicit `forall` will be scoped over any accompanying definitions, like `pair`.

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

example :: forall a. a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: [a]
    pair = [x, x]
```

The above are just two of many examples where `ExplicitForAll` is useful.
Other extensions that benefit from the usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes`, and many more.

## Existential quantification

As said earlier, besides universal quantification, Haskell also supports existential quantification.
This too can be done with the `forall` keyword.

You can do it this way because these two constructions – `(exists x. p x) -> q` and `forall x. (p x -> q)` – are equivalent in terms of first-order predicate logic.
For a theoretical proof of this statement, you can check this [thread](https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types?rq=1).

In this section, we'll look at existential quantification in data types and function signatures.

### Existential quantification in data types

First, to declare existentially quantified data types, you need to enable either the [`ExistentialQuantification`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html) or the [`GADTs`](https://downloads.haskell.org/~ghc/6.4/docs/html/users_guide/gadt.html) extension.

A classic motivating example are heterogeneous lists.
Haskell's `[]` type represents a homogeneous list: a list whose elements are all of the same type.
You can't have a list where the first element is an `Int` and the second is a `String`.

However, you can emulate a heterogeneous list by wrapping all the elements in an existential type.

You can define an existential type by using `forall` on the left side of the constructor name.

```haskell
data Elem = forall a. MkElem a
```

This effectively "hides" the element's type `a` inside the `MkElem` constructor.

```haskell
multitypedList :: [Elem]
multitypedList = [MkElem "a", MkElem 1, MkElem (Just 5)]
```

This is not very useful, though.
When we pattern match on `MkElem`, the type of the inner value is not known at compile time.

```hs
useElem :: Elem -> Int
useElem (MkElem (x :: Int)) = x + 1
                 ^^^^^^^^
-- • Couldn't match expected type ‘a’ with actual type ‘Int’
```

There's nothing we can do with it, not even print it to stdout.

```hs
printElem :: Elem -> IO ()
printElem (MkElem x) =
  print x
  ^^^^^^^
-- • No instance for (Show a) arising from a use of ‘print’
--   Possible fix:
--     add (Show a) to the context of the data constructor ‘MkElem’
```

GHC kindly suggests adding a `Show` constraint to the `MkElem` constructor.
Now, when we match on `MkElem`, the `Show` instance is brought into scope, and we can print our values.

```haskell
data Elem = forall a. (Show a) => MkElem a

multitypedList :: [Elem]
multitypedList = [MkElem "a", MkElem 1, MkElem (Just 5)]

printElem :: Elem -> IO ()
printElem (MkElem x) =
  -- We can use `print` here because we have a `Show a` instance in scope.
  print x

main :: IO ()
main = forM_ multitypedList printElem

λ> "a"
λ> 1
λ> Just 5
```

Note that, since the type variable `a` is "hidden" inside the `MkElem` constructor,
you can only use the constraints declared in the constructor.
You can't constrain it any further.

```haskell
allEqual :: Eq ??? => [Elem] -> Bool -- There is no type variable that you can add a constraint to.
allEqual = ...
```

Another useful example of hiding type variables is the `SomeException` wrapper.

```haskell
data SomeException = forall e. Exception e => SomeException e
```

All exceptions, upon being thrown, are wrapped in `SomeException`.
If you want to catch all thrown exceptions, you can use `catchAll` to catch a `SomeException`.

When you pattern match on the `SomeException` constructor, its `Exception` instance is brought into scope.
`Exception` [implies `Typeable` and `Show`](https://hackage.haskell.org/package/base/docs/Control-Exception.html#t:Exception),
so those instances are also brought into scope.

```hs
exceptionHandler :: SomeException -> IO ()
exceptionHandler (SomeException (ex :: e)) =
  -- The following instances are in scope here:
  -- `Exception e`, `Typeable e`, `Show e`
  ...
```

These three instances are all the information we have about `ex` at compile time.
Luckily, the `Typeable` instance we have in scope lets us perform a
[runtime cast](https://hackage.haskell.org/package/base/docs/Data-Typeable.html#v:cast),
and that's exactly what functions like `catch` and `fromException` do under the hood.

### Existential quantification in function signatures

Type variables of a function can also be existentially quantified.

Before proceeding further, let's first have a look at higher-rank types.

<hr>

**Higher-rank types**

By default, Haskell98 supports rank-0 and rank-1 types.

Rank-0 types are just monomorphic types (also called monotypes), i.e. they don't have any type variables:

```hs
f :: Int -> Int
```

Rank-1 types have a `forall` that does not appear to the left of any arrow.
The type variables bound by that `forall` are universally quantified.

```hs
f :: forall a. a -> a

-- Keep in mind that the `->` operator has precedence over `.`
-- so this is equivalent to:
f :: forall a. (a -> a)
```

By enabling the [`RankNTypes`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html) extension, we unlock _higher-rank_ types.
Just like higher-order functions take other functions as arguments,
higher-rank types take lower-rank types as arguments.

Rank-2 types take a type of rank 1 (but no higher) as an argument.
In other words, they may have a `forall` that appears to the left of one arrow.
The type variables bound by that `forall` are _existentially_ quantified.

```hs
f :: (forall a. (a -> a)) -> Int
```

Similarly, rank-3 types take a type of rank 2 (but no higher) as an argument.
The `forall` appears to the left of two arrows.

Here, `a` is universally quantified.

```hs
f :: ((forall a. (a -> a)) -> Int) -> Int
```

In general:
* A rank-n type is a function whose highest rank argument is n-1.
* A type variable is universally quantified if it's bound by a forall appearing to the left of an even number of arrows.
* A type variable is existentially quantified if it's bound by a forall appearing to the left of an odd number of arrows.

Here's another example for good measure:

```hs
f :: forall a. a -> (forall b. Show b => b -> IO ()) -> IO ()
```

Here, `f` is a rank-2 type with two type variables: a universal type variable `a` and an existential type variable `b`.

<hr>

But what does it _mean_ for a type variable to be existentially quantified?

In short: whereas universally quantified type variables are instantiated at the "use site" (i.e. the _user_ of the function has to choose which type they want that type variable to be),
existentially quantified type variables are instantiated at the "definition site" (where the function is defined).
In other words, the function's definition is free to choose how to instantiate the type variable; but the user of the function is not.

Let's look at a real-life example.
Imagine a function that prints logs while calculating some result.
You may want to write logs to the console or to some file.
In order to abstract over where the logs are written to, you pass a logging function as an argument.

```haskell
func :: _ -> IO ()
func log = do
  username <- getUsername
  log username

  userCount <- getUserCount
  log userCount

getUsername :: IO String
getUsername = undefined

getUserCount :: IO Int
getUserCount = undefined

main :: IO ()
main = do
  -- log to the console
  func print

  -- log to a logfile
  func (appendFile "logfile.log" . show)
```

What type should the `log` function have?
Since it logs both `String` and `Int`,
one might think that adding a function with signature `a -> IO` and a constraint `Show a` would be enough to implement it.

```haskell
log :: Show a => a -> IO ()
```

However, if we try this:

```haskell
func :: Show a => (a -> IO ()) -> IO ()
func log = ...
```

We will see this compiler error:

```none
  log username
      ^^^^^^^^
Couldn't match expected type `a` with actual type `String`...
```

We get this error because:

1. `func`'s type variable `a` is universally quantified, therefore it can only be instantiated at the use site.
2. We're trying to instantiate it at `func`'s definition site (`log username` is trying to instantiate the type variable `a` with `String` and `log userCount` is trying to instantiate the type variable `b` with `Int`).

Because we want `a` to be instantiated at `func`'s definition site (rather than its use site), we need `a` to be existentially quantified instead.

```haskell
{-# LANGUAGE RankNTypes #-}

func :: (forall a. Show a => a -> IO ()) -> IO ()
```
Now the compiler knows that no matter which function we pass to `func`, it must work for _any_ type `a`,
and `func`'s definition will be responsible for instantiating it.

```haskell
func :: (forall a. Show a => a -> IO ()) -> IO ()
func log = do
  username <- getUsername
  log @String username

  userCount <- getUserCount
  log @Int userCount
```

Note that type applications aren't strictly necessary here,
the compiler is smart enough to infer the right types,
but they do help with readability.

### Connection between existential types and existentially quantified variables

Lastly, we should point out that existential types are isomorphic to functions with existentially quantified variables
(i.e. they're equivalent), hence their name.

For example, the `Elem` type we saw earlier is equivalent to the
following [CPS](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style) function:

```hs
data Elem = forall a. Show a => MkElem a

type Elem' = forall r. (forall a. Show a => a -> r) -> r
```

And we can prove this isomorphism:

```hs
elemToElem' :: Elem -> Elem'
elemToElem' (MkElem a) f = f a

elem'ToElem :: Elem' -> Elem
elem'ToElem f = f (\a -> MkElem a)
```

## Summary

We have taken a look at universal and existential quantification in Haskell.
With a few extensions and the `forall` keyword,
quantification allows you to further expand the rich type system of the language and to express ideas that were previously impossible.

For more Haskell tutorials, you can check out our [Haskell](https://serokell.io/blog/haskell) section or follow us on [Twitter](https://twitter.com/serokell).

If you see any issues with the text of the blog or need some help understanding it, you're welcome to submit an issue in blog's [GitHub repo](https://github.com/serokell/blog-posts).

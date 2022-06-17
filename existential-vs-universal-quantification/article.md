# Universal and Existential Quantification in Haskell

In logic, there are two common symbols: universal quantifier and existential quantifier.

You might recognize them as $\forall$ (for all) and $\exists$ (there exists).

The concepts these symbols stand for are relevant to Haskellers as well, since both universal and existential quantification is possible in Haskell.

In this article, we will show how to explicitly write down both types of quantification via the `ExplicitForAll` extension.

<!-- TODO: Introduce the contents of the article when reordering is finished -->

## Universal quantification

In Haskell, all type variables are universally quantified by default. It just happens implicitly.

As an example, look at the `id` function.

```haskell
id :: a -> a
```

We can think of it as "for all types $a$, this function takes a value of that type and returns a value of the same type".

With the `ExplicitForAll` extension, we can write that down explicitly.

```haskell
{-# LANGUAGE ExplicitForAll #-}

id :: forall a. a -> a
```

The syntax is simple – at the beginning of function or instance declaration, before any constraints or arguments are used, use
the `forall` quantifier to introduce all type variables that you will use later.

Here, we introduce four variables: `a`, `b`, `c`, and `m`:

```Haskell
func :: forall a b c m. a -> b -> m c -- OK, it compiles
```

All type variables that you introduce in your type signature should belong to exactly one `forall`. No more, no less.

```haskell
func :: forall a b c.   a -> b -> m c -- Error, type variable `m` is not introduced
```

Of course, we can also add constraints.
For example, we might want to specify that the `m` variable needs an instance of the `Monad` typeclass.

```Haskell
func :: forall a b c m. Monad m => a -> b -> m c
```
So far, it might seem not very useful.
Nothing changed when we added the quantifier because it was already there (although implicit).
On its own, `ExplicitForAll` doesn't do a lot.

However, making the quantification explicit allows us to do some new things.
You frequently need it to work with other extensions.
Some of them will work better, and some of them just won't work at all without the `forall` quantifier. :)

### Practical use cases of universal quantification

#### Reordering type variables

**Note:** Here and in the future examples, we will use the [`TypeApplications`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html extension, which allows you to pass types as function arguments via the `@` sign. (If specifying concrete type is unnecessary, you can use the type wildcard `@_`.)

With `ExplicitForAll`, you can change the order of type variables.

How can that be useful?
Let's imagine you want to define a function with 10 type variables (yes, there are real-life [examples](https://hackage.haskell.org/package/leancheck-0.9.10/docs/Test-LeanCheck-Utils-TypeBinding.html#v:-45--62--62--62--62--62--62--62--62--62--62--62--62-:) of such functions).
And when you use it, you want to specify the type of the last argument explicitly.

By default, the order of type variables that you assign types to is the same as the order in which they are introduced in the function definition.

Without `forall`, you would write something like this:

```Haskell
veryLongFunction :: a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @_ @_ @_ @_ @_ @_ @_ @_ @_ @Integer ...
```

Quite long, right?
However, this can be simplified with explicit `forall`:

```Haskell
veryLongFunction :: forall j a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @Integer ...
```

Since `j` is the first type variable in the declaration of `veryLongFunction`, you can specify only it.
This is useful not only in such big examples but also when your function requires any of its arguments to be specified on usage.

### Supporting `ScopedTypeVariables`

A common extension that needs `ExplicitForAll` is [`ScopedTypeVariables`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/scoped_type_variables.html#), which allows you to pass a type variable from an outer function to one defined in a where clause.

The code below will not compile because the compiler thinks that the `a` in `example`'s signature and the `a` in `pair`'s are two different types.

```haskell
example :: a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: [a]
    pair = [x, x]
```

To fix it, you need to use the `ScopedTypeVariables` extension and also add a `forall` quantifier to the `example` function, since the extension won't work without it.

```haskell
example :: forall a. a -> [a] -> [a]
```

The above are just two of many examples where `ExplicitForAll` is useful.
Other extensions that somehow benefit from the usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes`, and many more.

## Existential quantification

As said earlier, besides universal quantification, Haskell also supports existential quantification.
This too can be done with the `forall` keyword.

You can do it this way because these two constructions – `(exists x. p x) -> q` and `forall x. (p x -> q)` – are equivalent in terms of first-order predicate logic.
For a theoretical proof of this statement, you can check this [thread](https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types?rq=1).

In this article, we'll look at existential quantification in data type and function signatures.

### Existential quantification in data types

First, to declare existentially quantified data types, you need to enable either the [`ExistentialQuantification`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html) or the [`GADTs`](https://downloads.haskell.org/~ghc/6.4/docs/html/users_guide/gadt.html) extension.

You can introduce an existentially quantified type variable in a data type by using `forall` on the left side of the constructor name.

```haskell
data Elem = forall a. Elem a
```

Existential types in data types allow you, for example, to have field in a data type without specifying its type.

```haskell
multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]
```

The above is a heterogeneous list that contains values of different types.
You can't do that with an universally quantified list because every element in that list would need to have the same type.

But, there are not many things you can do with elements of this list.
Since the type of elements is unknown at the use site, you can't address it, nor can you pass it to functions that expect values of concrete type.

Adding constraints to the `a` variable may improve the situation:

```haskell
data Elem = forall a. (Show a) => Elem a

multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]

-- We can use 'print' here because inner types of 'Elem' has 'Show' constraint
printElem :: Elem -> IO ()
printElem (Elem x) = print x

main :: IO ()
main = fmap printElem multitypedList

λ> "a"
λ> 1
λ> Just 5
```
Note that since type variable `a` is hidden with a quantifier, you can't add constraints to it besides those added to the constructor.

```haskell
showFunc :: Show ??? => [Elem] -> String -- There is no type variable, that you can add constraint to
showFunc (x:xs) => (show x) ++ (showFunc xs)
```

A useful example of hiding type variables is the `SomeException` wrapper.

```haskell
data SomeException = forall a. Exception a => SomeException a
```

It allows you to catch exceptions of any type.
As long as you haven't specified its type (and it is not obvious to the compiler, for example, from the handler), all exceptions will be caught, wrapped in `SomeException`.

However, again, there is a limitation.
We know nothing about the inner exception.
We can, for example, print it, but sometimes we need more information.
And we can't use type inheritance to describe what type we want to get.

```haskell
-- Something like that won't work.
-- We know too little about type inside,
-- to be sure that it is 'MyException'.
myFromException :: SomeException -> MyException
myFromException (SomeException (ex :: MyException)) = ex
```

Luckily for users, module `Control.Exception` provides special function `fromException`, the purpose of which is to extract the inner value and cast to a specified type. But how that is done is a topic for a different article.

### Existential quantification in function signatures

A function's type variables can also be existentially quantified.

Before proceeding further, let's first have a look at higher-rank types.

<hr>

**Higher-rank types**

By default, Haskell98 supports rank-0 and rank-1 types.

Rank-0 types are just monomorphic types (also called _monotypes_), i.e. they don't have any type variables:

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
Just like higher-order functions are functions that take other functions as arguments,
higher-rank types take lower-rank types as arguments.

Rank-2 types take a type of rank 1 (but no higher) as an argument.
In other words, they may have a `forall` that appears to the left of 1 arrow.
The type variables bound by that `forall` are _existentially_ quantified.


```hs
f :: (forall a. (a -> a)) -> Int
```

Similarly, rank-3 types take a type of rank 2 (but no higher) as an argument.
The `forall` appears to the left of 2 arrows.
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

Here, `f` is a rank-2 type with 2 type variables: a universal type variable `a` and an existential type variable `b`.

But what does it _mean_ for a type variable to be existentially quantified?

In short: whereas universally quantified type variables are instantiated at the "use site" (i.e. the _user_ of the function has to choose which type they want that type variable to be),
existentially quantified type variables are instantiated at the "definition site" (where the function is defined).
In other words, the function's definition is free to choose how to instantiate the type variable; but the user of the function is not.

Let's look at a real life example.
Imagine a function that prints logs while calculating some result.
You may want to write logs to the console or into some file.
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

Note that type applications isn't strictly necessary here,
the compiler is smart enough to infer the right types,
but it does help with readability.

## Summary

We have taken a look at universal and existential quantification in Haskell.
With a few extensions and the `forall` keyword,
quantification allows you to further expand the rich type system of the language and to express ideas that were previously impossible.

For more Haskell tutorials, you can check out our [Haskell](https://serokell.io/blog/haskell) section or follow us on [Twitter](https://twitter.com/serokell).

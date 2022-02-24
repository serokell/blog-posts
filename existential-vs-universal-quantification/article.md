# Existential and Universal Quantification in Haskell

This article gives a brief introduction into one of the most common Haskell extensions – `ExplicitForAll` – and syntax constructions that this extension brings to language.
It is used in about all big haskell projects, so it is very nice to either become acquainted with it or to refresh it in your memory if you already know about it.

Stay tuned if you want to know how to create lists of differently typed variables, how to add complex constraints, and more!


## Introduction

By default quantification in Haskell is implicit. Without special extension - `ExplicitForAll`, quantifiers will stay hidden. However, enabling it allows us to see the true functions types, like for example, the type of `id`.

```haskell
id :: forall a. a -> a
id x = x
```

This quantifier comes from predicate logic.
From there we also know about existence of the `exists` quantifier.
While, there were some attempts to use it [in the past](https://web.archive.org/web/20060304191310/https://hackage.haskell.org/trac/haskell-prime/wiki/ExistentialQuantification#Syntaxofexistentials), it was removed from the compiler, in favor of using only `forall` one.

The key point of both universal and existential quantification is choosing where, we are going to instantiate a type variable - where it is defined, or where it is used.

## Universal quantification and `ExplicitForAll`

By default, all type variables are considered universally quantified.
It means that what type variable means is chosen, when you call a function.
For example, every time you call `id` in other functions, type variable `a` is specified with concrete type.

### Usage

Let's try to add `forall` manually.
The syntax is very simple – at the beginning of function or instance declaration, before any constraints or arguments are used, use
the quantifier to introduce all type variables you want to use later:

```Haskell
func :: forall a b c m. a -> b -> m c -- OK, it compiles
```
Here we introduce four variables.
All type variables that you introduce in your definition should belong to one `forall` exactly, though they may not belong to the same one. (I will explain it further.)

```haskell
func :: forall a b c.   a -> b -> m c -- Error, type variable `m` is not introduced
```

Lets add some constraints.
For example, we might want to specify the `m` variable here as an instance of `Monad` typeclass.

```Haskell
func :: forall a b c m. Monad m => a -> b -> m c
```
You might want to ask, what's so special about this syntax?
Nothing changed since we added the quantifier because it was already there!
However, it allows us to use new tricks.

### Examples

#### Order of type variables

First of all, you can now change the order of type variables.
Let's imagine you want to define a function with 10 type variables (yeah, there are real-life [examples](https://hackage.haskell.org/package/leancheck-0.9.10/docs/Test-LeanCheck-Utils-TypeBinding.html#v:-45--62--62--62--62--62--62--62--62--62--62--62--62-:) of such functions), and you know that when you use it, you want to specify type of the last argument explicitly.
To pass a type as a parameter to a function, we will use the `TypeApplication` extension.
Few notes about passing types to functions. Using `TypeApplication` extension you can pass types as function arguments, using `@` sign (If specifying concrete type is unnecessary you can use type wildcard `@_`).
By default, the order of type variables, which you assign types to, is the same as the order in which they are introduced in the function definition.
Without `forall`, you would write something like this:

```Haskell
veryLongFunction :: a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @_ @_ @_ @_ @_ @_ @_ @_ @_ @Integer ...
```
Quite long, right?
However, by using explicit `forall`, this can be simplified:

```Haskell
veryLongFunction :: forall j a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @Integer ...
```

Since `j` is the first type variable in the declaration of `veryLongFunction` you can specify only it.
This is useful not only in such big examples but also when your function requires any of its arguments to be specified on usage.

### Support of other extensions

`ExplicitForAll` shines most when you need to enable other extensions – some of them just won't work without the `forall` quantifier. :)
The most common one is `ScopedTypeVariables`, which allows you to pass a type variable from an outer function to the one defined in a where clause.

```haskell
example :: a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: a -> [a]
    pair x = [x, x]
```
This will cause an error without `ScopedTypeVariables`, because compiler thinks that `a` in `example`'s signature and `a` in `double`'s are two different types.
To fix it, you also need to use extension and add a `forall` quantifier, as extension won't work without it.

```haskell
example :: forall a. a -> [a] -> [a]
```
You might also notice how it was useful with the `TypeApplication` extension.
Other extensions that somehow benefit from the usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes`, and many more.

## Existential quantification
As I said earlier, besides universally quantified variables, Haskell also supports existential types.
Remember, that we still use the `forall` keyword ¯\\\_(ツ)_/¯.
Actually, it is implemented this way, because in terms of first-order predicate logic these two constructions are equivalent: `(exists x. p x) -> q` and `forall x. (p x -> q)`.
If you want a theoretical proof of this statement, you can check this [tread](https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types?rq=1).

### Usage

In order to proceed further, we'll have to enable the `RankNTypes` extension. This lets us write higher-rank types.

By default, all types are rank-1. In a rank-1 type, a `forall` cannot appear on the left of an arrow (keep in mind that the `->` operator has precedence over `.`)

For example, this function have rank-1 types:

```haskell
f :: forall a b. a -> b -> a
```

With the `RankNTypes` extension, we can write rank-2 types - types where a `forall` appears to the left of one arrow:

```haskell
f :: (forall a. Show a => a -> IO ()) -> IO ()
```

Because forall a appears to the left of one arrow, `a` is said to be existentially quantified.
It is also true for every `forall` that is introduced to the left of an odd number of functional arrows.
But what does this mean?

In contrast to the universal quantifier, the existential one means that the function is itself responsible for instantiating what a type variable should mean. So the result type of existentially quantified variable will become known when we address it at the 'definition-site' (i.e., in the function body).
This also means that no one can specify its type outside of function.

Let's take a closer look on practical examples.

We will start with functions:

```haskell
func :: forall a c. a -> (forall b. (Show b) => b -> IO ()) -> IO c
```
There are a few things to mention here.

* In the type signature, `a` and `c` are universally quantified variables, and `b` is existentially quantified.
* The same variable can't be quantified more than once.
* You can see that constraints for `b` are added in the middle of the declaration of the function type (because `b` was introduced in the middle of the function).
* You can choose type for `b` only inside `func` definition, and you can't do it when you call `func` (we will discuss it further below).

Here's a real-life example of using existential types in functions.
Imagine a function that prints some logs while calculating the result.
You may want to write logs into a console or into some file.
In order to do it, you pass a logging function as an argument.

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
```

What type should the log function have?
Since it somehow logs both `String` and `Int`:

```haskell
log :: Show a => a -> IO ()
```
However, if we add this signature to `func` and try to call it with different arguments:

```haskell
func :: Show a => (a -> IO ()) -> IO ()
func log = ...

main :: IO ()
main = do
  -- log to the console
  func print

  -- log to a logfile
  func (appendFile "logfile.log" . show)
```
We will see some nasty compiler errors:
```
Couldn't match expected type `a` with actual type `String`...
```
It is happening, because inside of `func`, `a` is not known yet. I will be known, only when you call `func` somewhere.
The only solution here is to add constraints and `forall` quantifier to the log function signature:

```haskell
{-# LANGUAGE RankNTypes #-}

func :: (forall a. Show a => a -> IO ()) -> IO ()
```
Now the compiler knows, that whatever function we pass to `func`, type variable `a` should be instantiated here, not outside.
After this change, we can specify.

With this change we can instantiate `a` when we call log.

```haskell
func :: (forall a. Show a => a -> IO ()) -> IO ()
func log = do
  username <- getUsername
  log @String username

  userCount <- getUserCount
  log @Int userCount
```

Note, that this type applications is unnecessary.
Compiler is smart enough to choose appropriate types.

#### Hiding type variables

Let's turn to datatypes now.
You can introduce existentially quantified type variable in the datatype, by using `forall` on the left side of equality sign.

```haskell
data MyData a = forall b. MkMyData b a
-- You need to enable either `ExistentialQuantification` or `GADTs` extension to use this syntax
```

The first thing you might have noticed is that we don't have a type variable for `b` on the left side of the expression.
However, we still can use it on the right side.
This is because `b` is existentially quantified.
The compiler will instantiate the type of the variable itself, and you can't specify it.

```haskell
func :: MyData Integer -> IO ()
func (MkMyData (x :: Double) y) = print x -- compiler error - can't match type of x with Double
```

#### Heterogeneous lists

Now we understand that existential types allow you to introduce type variables without the necessity to specify them.

This might be useful when building a multi-typed list:

```haskell
data Elem = forall a. Elem a

multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]
```
This is a heterogeneous list that contains values of different types.
You can't do that with an universally quantified list because every element in that list would need to have the same type.w
But, there are not many things you can do with elements of this list.
Since the type of elements is unknown, you can't address it, nor can you pass it to functions that expect values of concrete type.
However, adding constraints to the `a` variable may improve the situation:

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

We hide exception type in the same way in the well-known `SomeException` wrapper:

```haskell
data SomeException = forall a. Exception a => SomeException a
```

It allows you to catch exceptions of any type.
As long, as you hadn't specified its type, (and it is not obvious to the compiler, from the handler for example), all exceptions will be caught, wrapped in `SomeException` datatype.
After that, you can use `fromException` function, to check, that you caught something you wanted to catch.


## Summary

We have taken a look at quantification in Haskell.
While we didn't introduce many syntax constructions, quantification allows you to further expand the rich type system of the language
and to create constructions that previously were impossible.

For more Haskell tutorials, you can check out our [Haskell](https://serokell.io/blog/haskell) section or follow us on [Twitter](https://twitter.com/serokell).

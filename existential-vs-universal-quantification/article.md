# Existential and Universal Quantification with ExplicitForAll

In this article, I give a brief introduction to one of the most common Haskell extensions – `ExplicitForAll` – and syntax constructions that this extension brings to language.

This extension is used in most large Haskell projects, so you'll need to become acquainted with it at one point or another.

Stay tuned if you want to know how to create lists of differently typed variables, how to add complex constraints, and more!

## Introduction

By default, quantification in Haskell is implicit.

Without the `ExplicitForAll` extension, you can't write quantifiers. Moreover, all variables are universally quantified, and existential quantification is turned off.
The key point of both universal and existential quantification is choosing where, we are going to instantiate a type variable - where it is defined, or where it is used.

## Universal quantification and `ExplicitForAll`

By default, all type variables are considered universally quantified.
This means that every time you use a function like `id`, you can _instantiate_ the universally quantified type variable `a` to (i.e., replace it with) any type.

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

Let's add some constraints.
For example, we might want to specify the `m` variable here as an instance of `Monad` typeclass.

```Haskell
func :: forall a b c m. Monad m => a -> b -> m c
```
You might want to ask, what's so special about this syntax?
Nothing changed when we added the quantifier because it was already there (although implicit).
However, making it explicit allows us to do some new things.

### Examples

#### Order of type variables

First of all, you can now change the order of type variables.
Let's imagine you want to define a function with 10 type variables (yes, there are real-life [examples](https://hackage.haskell.org/package/leancheck-0.9.10/docs/Test-LeanCheck-Utils-TypeBinding.html#v:-45--62--62--62--62--62--62--62--62--62--62--62--62-:) of such functions), and you know that when you use it, you want to specify the type of the last argument explicitly.

Here and in the future examples, we will use the `TypeApplication` extension, which allows you to pass types as function arguments via the `@` sign. (If specifying concrete type is unnecessary, you can use the type wildcard `@_`.)

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

### Support of other extensions

`ExplicitForAll` shines most when you need to enable other extensions. Some of them just won't work without the `forall` quantifier. :)

The most common one is `ScopedTypeVariables`, which allows you to pass a type variable from an outer function to one defined in a where clause.

```haskell
example :: a -> [a] -> [a]
example x rest = pair ++ rest
  where
    pair :: [a]
    pair = [x, x]
```

This will cause an error without `ScopedTypeVariables` because compiler thinks that the `a` in `example`'s signature and the `a` in `pair`'s are two different types.
To fix it, you also need to use extension and add a `forall` quantifier to `example` function, as extension won't work without it.

```haskell
example :: forall a. a -> [a] -> [a]
```
You might also notice how it was useful with the `TypeApplication` extension.
Other extensions that somehow benefit from the usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes`, and many more.

## Existential quantification

As I said earlier, besides universally quantified variables, Haskell also supports existential types.
But they also use the `forall` keyword.

It can be implemented this way because these two constructions – `(exists x. p x) -> q` and `forall x. (p x -> q)` – are equivalent in terms of first-order predicate logic.
For a theoretical proof of this statement, you can check this [thread](https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types?rq=1).

### Usage

In order to proceed further, we'll have to enable the `RankNTypes` extension. This lets us write higher-rank types.

<hr>

**Higher-rank types**

By default, all types are rank-1. In a rank-1 type, a `forall` cannot appear on the left of an arrow (keep in mind that the `->` operator has precedence over `.`)

For example, this function has a rank-1 type:

```haskell
f :: forall a b. a -> b -> a

-- Or, to make the precedence explicit:
f :: forall a b. (a -> b -> a)
```

With the `RankNTypes` extension, we can write rank-2 types – types where `forall` appears to the left of one arrow:

```haskell
f :: (forall a. Show a => a -> IO ()) -> IO ()
```

<hr>

Because `forall a` appears to the left of one arrow, `a` is said to be existentially quantified.
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

* The same variable can't be quantified more than once.
* In the type signature, `a` and `c` are universally quantified variables, and `b` is existentially quantified.
* You can see that constraints for `b` are declared _after_ `b` has been introduced with the `forall` quantifier
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
So one might think that adding a function with signature `a -> IO` and a constraint `Show a` would be enough to implement it.

However, if we try this:

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

We will see some nasty compiler errors.

```none
  log username
      ^^^^^^^^
Couldn't match expected type `a` with actual type `String`...
```

We're getting this error because:

1. `func`'s type variable `a` is universally quantified, therefore it can only be instantiated at the use-site.
2. We're trying to instantiate it at `func`'s definition-site (`log username` is trying to instantiate the type variable `a` with `String` and `log userCount` is trying to instantiate the type variable `b` with `Int`).

Because we want `a` to be instantiated at `func`'s definition-site (rather than its use-site), we need `a` to be existentially quantified instead.

```haskell
{-# LANGUAGE RankNTypes #-}

func :: (forall a. Show a => a -> IO ()) -> IO ()
```
Now the compiler knows that no matter which function we pass to `func`, type variable `a` should be instantiated here, not outside.

With this change, we can instantiate `a` when we call log.

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

#### Heterogeneous lists

Now we understand that existential types allow you to introduce type variables without the necessity to specify them.
Let's take a look how this is done in datatypes.

You can introduce existentially quantified type variable in a datatype by using `forall` on the left side of the constructor name. 
Existential types in datatypes allow you to have field in a datatype without specifying its type.

```haskell
data Elem = forall a. Elem a

multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]
```

This is a heterogeneous list that contains values of different types.
You can't do that with an universally quantified list because every element in that list would need to have the same type.

But, there are not many things you can do with elements of this list.
Since the type of elements is unknown at the use-site, you can't address it, nor can you pass it to functions that expect values of concrete type.
`Data.Typeable` module's functionality can help to circumvent this restriction. It allows you to cast one type (even unknown) to another one.

Adding constraints to the `a` variable may also improve the situation:

```haskell
-- You need to enable either `ExistentialQuantification` or `GADTs` extension to use this syntax
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

Another useful example of hiding type variables is the `SomeException` wrapper.

```haskell
data SomeException = forall a. Exception a => SomeException a
```

It allows you to catch exceptions of any type.
As long, as you hadn't specified its type, (and it is not obvious to the compiler, from the handler for example), all exceptions will be caught, wrapped in `SomeException` datatype.

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

Luckily for users, module `Control.Exception` provides special function `fromException`, the purpose of which is to extract the inner value and cast to a specified type. How it is done is a topic for a different article.

## Summary

We have taken a look at quantification in Haskell.
While we didn't introduce many syntax constructions, quantification allows you to further expand the rich type system of the language
and to create constructions that previously were impossible.

For more Haskell tutorials, you can check out our [Haskell](https://serokell.io/blog/haskell) section or follow us on [Twitter](https://twitter.com/serokell).

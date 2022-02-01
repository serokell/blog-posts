# Existential and Universal Quantification in Haskell

This article gives a brief introduction into one of the most common Haskell extensions – `ExplicitForAll` – and syntax constructions that this extension brings to language. 
I haven't yet found any big project that wouldn't use this extension, so it is very
useful to either become acquainted with it or to refresh it in your memory if you already know about it.

Stay tuned if you want to know how to create lists of differently typed variables, how to add complex constraints, and more!


## Introduction

From predicate logic, we know about the existence of two quantifiers – `$\forall$` and `$\exists$` – that we can apply to a term `x` and some statement.
The former means: "For every term x, the statement is true", the later: `"There is at least one term x for which this statement is true"`.

Since Haskell is based on predicate logic, there is a place for these quantifiers in language syntax. However, in Haskell, we are talking not about some terms and statements but about types.

```haskell
id :: forall a. a -> a
id x = x
```

This quantification is hidden by default (if you check the type signature of `id`, there is no quantifier there). To see it, you need to use a specific extension – "ExplicitForAll".

## Universal quantification and `ExplicitForAll`

The key point of both universal and existential quantification is choosing who is responsible for deducing types of a function, datatype, or class.
By default, all type variables are considered universally quantified.
It means that **the caller** of a function (so, the programmer, actually) is responsible in case the type variable is ambiguous.

### Usage

Let's try to add `forall.` manually.
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

We might want to introduce some constraints.
For example, we might want to specify that the `m` variable here is an instance of `Monad` typeclass.
In order to add this constraint, we need to place it after all variables are declared:

```Haskell
func :: forall a b c m. Monad m => a -> b -> m c
```
You might want to ask, what's so special about this syntax?
Nothing changed since we added the quantifier because it was already there!
However, there are a few things to notice here.

### Examples

#### Order of type variables

First of all, you can now change the order of type variables.
Let's imagine you want to define a function with 10 type variables (yeah, there are real-life examples of such functions), and you know that when you use it, you want to specify the last argument explicitly.
To pass a type as a parameter to a function, we will use the `TypeApplication` extension.
Without `forall`, you would write something like this:

```Haskell
veryLongFunction :: a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @_ @_ @_ @_ @_ @_ @_ @_ @_ @Integer ...
```
Quite long, right?
This is because GHC will maintain the same order as when these variables first appear in the function definition.
However, by using explicit `forall.`, this can be simplified:

```Haskell
veryLongFunction :: forall j a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @Integer ...
```

Since `j` is the first type variable in the declaration of `veryLongFunction` you can specify only it.
This is useful not only in such big examples but also when your function requires any of its arguments to be specified on usage.

### Support of other extensions

`ExplicitForAll` shines most when you need to enable other extensions – some of them just won't work without the `forall.` quantifier. :) 
The most common one is `ScopedTypeVariables`, which allows you to pass a type variable from an outer function to the one defined in a where clause. 

```haskell
example :: a -> [a] -> [a]
example x rest = double ++ rest
  where
    double :: a -> [a]
    double x = [x, x]
```
This will cause an error without `ScopedTypeVariables`. But to use it, you need to add a `forall.` quantifier.

```haskell
example :: forall a. a -> [a] -> [a]
```
You might also notice how it was useful with the `TypeApplication` extension.
Other extensions that somehow benefit from the usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes`, and many more.

## Existential quantification
Besides universally quantified variables, Haskell also supports existential types.
But in order to use them ... you still need to use the `forall.` keyword ¯\\\_(ツ)_/¯.

The following construction is called an existential type in Haskell – when a type variable is introduced (using the `forall.` quantifier) inside the signature, not before it.
In contrast to the universal quantifier, the existential one means that the compiler (**callee**) is responsible for deducing what a type variable should mean.
This also means that the caller can't specify this type variable.

![When you use existential types](meme.jpg)


### Usage

We will start with functions:

```haskell
func :: forall a c. a -> (forall b. (Show b) => b) -> c
```
There are a few things to mention here.

* In the type signature, `a` and `c` are universally quantified variables, and `b` is existentially quantified. This is what I was talking about when I said that type variables may belong to different quantifiers.
* The same variable can't be quantified more than once.
* You can see that constraints for `b` are added in the middle of the declaration of the function type (because `b` was introduced in the middle of the function).
* Now you can't manually choose type for `b` – the compiler must deduce the correct variable you want to use by itself.

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
Ambiguous type variable ‘a1’ arising from a use of ‘func’
```

This is because the compiler can't deduce the type of `a` from the given constraints.
If we use `forall.` explicitly as we did before, it wants help, because in this case, the caller is the one who should specify the type of the log function. But we can't specify it because it is different for different logging operations.
The only solution here is to add constraints and `forall.` quantifier to the log function signature:

```haskell
{-# LANGUAGE RankNTypes #-}

func :: (forall a. Show a => a -> IO ()) -> IO ()
```
Now we put all responsibility for deducing types on the shoulders of the compiler, and it will deduce them the way it wants.

#### Hiding type variables

```Haskell
data MyData a = forall b. MkMyData b a
-- You need to enable either `ExistentialQuantification` or `GADTs` extension to use this syntax
```

The first thing you might have noticed is that we don't have a type variable for `b` on the left side of the expression.
However, we still can use it on the right side.
This is because `b` is existentially quantified.
The compiler will deduce the type of the variable itself, and you can't specify it.

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
You can't do that with an universally quantified list because every element in that list would need to have the same type.
But, there are not a lot of things you can do with elements of this list.
Since the type of elements is unknown, you can't address it, nor can you pass it to functions that expect values of concrete type.
However, adding constraints to the `a` variable may improve the situation:

```haskell
data Elem = forall a. (Show a) => Elem a

multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]

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

It allows you to catch exceptions of any type (presumed it was wrapped inside `SomeException`) and then check if this is an exception of the type you want.


## Summary

We have taken a look at quantification in Haskell.
While we didn't introduce many syntax constructions, quantification allows you to further expand the rich type system of the language 
and to create constructions that previously were impossible.

For more Haskell tutorials, you can check out our [Haskell](https://serokell.io/blog/haskell) section or follow us on [Twitter](https://twitter.com/serokell).

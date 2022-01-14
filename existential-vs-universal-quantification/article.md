# Existential vs universal quantification

## Introduction
This article gives brief information about one of the most common extension in Haskell projects, `ExplicitForAll` and syntax constructions that this extension brings to language.
Stay tuned if you want to know how to create lists of differently typed variables, how to add complex constraints and more!

From predicate logic we know about the existence of two quantifiers - `$\forall$` and `$\exists$`, that we can apply to a term `x` and some statement.
The former means: `"For every term x the statement is true"`, the later: `"There is at least one term x for which this statement is true"`.

Since Haskell is based on predicate logic there is a place for these quantifiers in language syntax, however, in Haskell we are talking not about some terms and statements, but about types.
```haskell
id :: forall a. a -> a
id x = x
```

This quantification is hidden by default (if you check `id` type signature, there is no quantifier there), without using a specific extension - "ExplicitForAll".

## Universal quantification and `ExplicitForAll`
The key point of both - universal and existential quantification is choosing, who is responsible for deducing types of a function, datatype or class.
By default, all type variables are considered universally quantified.
It  means that **caller** of a function (so programmer actually) is responsible in case type variable is ambiguous.

### Usage
Let's try to add `forall.` manually.
The syntax is very simple - at the beginning of function or instance declaration, before any constraints or arguments are used, use
the quantifier to introduce all type variables you want to use later:

```Haskell
func :: forall a b c m. a -> b -> m c -- OK, it compiles
```
Here we introduce four variables.
All type variables that you introduce in your definition should belong to one forall exactly, though they may not belong to the same one (I will explain further).

```haskell
func :: forall a b c.   a -> b -> m c -- Error, type variable `m` is not introduced
```

We might want to introduce some constraints.
For example, we might want to specify that the `m` variable here is an instance of `Monad` typeclass.
In order to add this constraint we need to place it after all variables are declared:

```Haskell
func :: forall a b c m. Monad m => a -> b -> m c
```
You might want to ask, what is so special about this syntax?
Nothing changed since we added the quantifier, because it was already there!
However, there are few things to notice here.

### Examples
#### Order of type variables
First of all, now you can change the order of type variables.
Let's imagine you want to define a function with 10 type variables (Yeah, there are real life examples of such functions), and you know that when you use it, you want to specify the last argument explicitly.
To pass type as parameter to a function, we will use the `TypeApplication` extension.
Without forall you will write something like this:

```Haskell
veryLongFunction :: a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @_ @_ @_ @_ @_ @_ @_ @_ @_ @Integer ...
```
Quite long right?
This is because ghc will maintain the same order, as these variables first appear in function definition.
However, using explicit `forall.` this can be simplified:

```Haskell
veryLongFunction :: forall j a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> j
veryLongFunction = ...

func = veryLongFunction @Integer ...
```

Since `j` is the first type variable in the declaration of `veryLongFunction` you can specify only it.
This is useful not only in such big examples, but also when your function requires any of its arguments to be specified on usage.

### Support of other extensions

`ExplicitForAll` shines most when you need to enable other extensions - some of them just want work without `forall.` quantifier)
The most common one is `ScopedTypeVariables`, which allows you to pass type variable from outer function, to the one defined in where clause
```haskell
example :: a -> [a] -> [a]
example x rest = double ++ rest
  where
    double :: a -> [a]
    double x = [x, x]
```
This will cause an error, without `ScopedTypeVariables`, but in order to use it, you need to add `forall.` quantifier.
```haskell
example :: forall a. a -> [a] -> [a]
```
You might also notice how it was useful with the `TypeApplication` extension.
Other extensions that somehow benefit from usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes` and many others.

## Existential quantification
Beside universally quantified variables, Haskell also supports existential types.
In order to use them ... you still need to use `forall.` keyword ¯\\\_(ツ)_/¯.
That might be a little confusing.
The following construction is called existential type in Haskell - when type variable introduced (using `forall.` quantifier) inside the signature, not before it
On the contrary to universal quantifier, existential one means, that compiler (**callee**) is responsible for deducing what type variable should mean.
This also means that the caller can't specify this type variable.

![When you use existential types](meme.jpg)


### Usage

We will start with functions:

```haskell
func :: forall a c. a -> (forall b. (Show b) => b) -> c
```
Few things to mention.
Here `a` and `c` are universally quantified and `b` is existentially quantified variables (that is what I was talking about, when said that type variables may belong to different quantifiers).
The same variable can't be quantified more than once.
Also, you can see that constraints for `b` are added in the middle of the function type declaration (because `b` was introduced in the middle of the function).
Moreover, now you can't manually choose type for `b` - the compiler must deduce the correct variable you want to use by itself.

There are some real-life examples of using existential types in functions.
Imagine a function that prints some logs, while calculating the result.
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
Howerver, if we add this signature to `func` and try to call it with different arguments:

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

This is because the compiler can't deduce the type of `a` from given constraints.
If we use `forall.` explicitly, like we have done before, it wants help, because in this case the caller is the one who should specify the type of log function, but we can't specify it, because it is different for different logging operations!!!
The only solution here is to add constraints and `forall.` quantifier to the log function signature:
```haskell
{-# LANGUAGE RankNTypes #-}

func :: (forall a. Show a => a -> IO ()) -> IO ()
```
Now we put all responsibility for deducing types on compiler shoulders, and it will deduce them, in a way it wants.

#### Hiding type variables
```Haskell
data MyData a = forall b. MkMyData b a
-- You need to enable either `ExistentialQuantification` or `GADTs` extension to use this syntax
```

The first thing you might have noticed is that we don't have a type variable for `b` on the left side of expression.
However, we still can use it on the right side.
This is because `b` is existentially quantified.
Compiler will deduce the type of the variable itself, and you can't specify it.

```haskell
func :: MyData Integer -> IO ()
func (MkMyData (x :: Double) y) = print x -- compiler error - can't match type of x with Double
```

#### Heterogeneous lists

Now we understand that existential types allow you to introduce type variables, without necessity to specify them.
This might be useful when building a multi-typed list:

```haskell
data Elem = forall a. Elem a

multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]
```
This is a heterogeneous list that contains values of different types.
You couldn't do that with universally quantified list, because in that case you should have every element in the list have the same type.
There are not many things you can do with elements of this list.
Since their type is unknown, you can't address it, nor can you pass it to functions that expect values of concrete type.
However, adding constraints to `a` variable may improve the situation

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

Note, that since type variable `a` is hidden with quantifier, you can't add constraints to it, beside those added to the constructor

```haskell
showFunc :: Show ??? => [Elem] -> String -- There is no type variable, that you can add constraint to
showFunc (x:xs) => (show x) ++ (showFunc xs)
```

The same way we hide exception type in well-known `SomeException` wrapper:
```haskell
data SomeException = forall a. Exception a => SomeException a
```

It allows you to catch exceptions of any type (presumed it was wrapped inside `SomeException`) and then check if this is an exception of the type you want.


## Summary

We had taken a look at quantification in Haskell.
While this hadn't introduced many syntax constructions, it allows you to further expand rich type system of the language,
and to create constructions, which previously were impossible.
This extension is often used in haskell projects (I actually hadn't yet found any big project without it), so it is very
useful either to become acquainted with it or to freshen it in your memory, if you already knew about it.

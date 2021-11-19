# Existential vs universal quantification

## Introduction
From type theory we know about existence of two quantifiers - `$\forall$` and `$\exists$`.
The former means: `"For every term x the statement is true"`, the later: `"There is at least one term x for which this statement is true"`.

Since Haskell is based on type theory there is a place for these quantifiers in language syntax, however, in Haskell we are talking not about some terms and statements, but about types. 
By default, all type variables are bounded by `forall` quantifier, which means that every type, that passes constraints, can be used inside this function/type declaration.
You can't see this quantification without using a specific extension - "Explicit Forall".

## Universal quantification and ExplicitForall
The mentioned extension is enabled by using this pragma:
```haskell
{-# LANGUAGE ExplicitForAll #-}
```
It allows us to use `forall.` quantifier in functions and types declarations manually.
By default, `forall.` is implicit and sometimes it can lead to unpleasant situations.
Consider this code:

```Haskell
prepend2 :: a -> [a] -> [a]
prepend2 x xs = pairFun x ++ xs
 where pairFun y = [y, y]
```
This code compiles without any errors, until you try to use variables from the outer function inside the `pairFun` function.
```Haskell
-- code compiles with error
prepend2 :: a -> [a] -> [a]
prepend2 x xs = pair ++ xs
 where pair :: [a]
       pair = [x, x]
```

If you try to compile this one you will get an error because compiler can't match type variable `a` from outer
declaration, with the one in inner declaration. 
This is happening, because the real signature of this function is this:

```Haskell
prepend2 :: forall a. a -> [a] -> [a]
prepend2 x xs = pair ++ xs
 where pair :: forall a. [a]
       pair = [x, x]
```

In order to correct the code you need to use `-XScopedTypeVariables` extension, which will only work if you
explicitly add forall keyword to `prepend2` function.

##### Usage
Let's try to add `forall.` manually.
The syntax is very simple - at the beginning of function or instance declaration, before any constraints or arguments were introduced use
quantifier with all type variables you want to introduce later (Note: you get a compilation error if you use undeclared
type variables after forall):

```Haskell
func :: forall a b c m. a -> b -> m c -- OK, it compiles
func :: forall a b c.   a -> b -> m c -- Error, type variable `m` is not introduced
```
Here we introduce four variables. We might want to introduce some constraints.
For example, it is very likely from the function definition that `m` is an instance of `Monad` typeclass.
In order to add this constraint we need to place it after all variables are declared:

```Haskell
func :: forall a b c m. Monad m => a -> b -> m c
```  
You might want to ask, what is so special about this syntax?
Nothing changed since we added the quantifier.
Well, actually there are few things to notice here.

##### Order of type variables
First of all, now you can change the order of type variables.
Let's imagine you want to define function with 10 type variables (Yeah, there are real life examples of such functions), and you know that when you use it, you want to specify the last argument explicitly.
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

##### Additional type variables
Second, you now can define functions with more type variables than you use on the right side of declaration.
This is used mostly when you want to add complex constraint, which require several types, without adding them into the definition itself:

``` Haskell
someMonadicFunction :: forall a m env. MonadReader env m => a -> m a
```
 
Here we don't have `env` used in the type definition, but despite that, we still able to add `MonadReader` constraint.

## Existential quantification
Beside universally quantified variables, Haskell also supports existential types. 
In order to use them ... you still need to use `forall.` keyword ¯\\\_(ツ)_/¯.
That might be a little confusing.
The following construction is called existential type in Haskell - when you use forall quantifier in the right-hand side of function or datatype declaration. 
But what is it used for? Let's find out!
We will start with functions:

```haskell
func :: forall a c. a -> (forall b. (Show b) => b) -> c
```
Few things to mention.
Here `a` and `c` are universally quantified and `b` is existentially quantified variables. 
The same variable can't be quantified more than once.
Also, you can see that constraints for `b` are added in the middle of the function type declaration.
This example is pretty useless.
The only thing that has changed here is that now you can't manually choose type for `b` - the compiler must deduce the correct variable you want to use by itself.

When we come to datatype declaration, it becomes more useful. 

```Haskell
data MyData a = forall b. MkMyData b a
```
The first thing you might have noticed is that we don't have type variable for `b` on the left side of expression.
However, we still can use it on the right side.
This is because `b` is existentially quantified.

```Haskell
MkMyData @_ @Integer -- This won't work, you don't have second variable in declaration
```
Compiler will deduce the type of the variable itself, and you can't specify it. 
It allows you to introduce variables, without need to specify them.
This might be useful for example in this case:

```haskell
data Elem = forall a. Elem a

multitypedList :: [Elem]
multitypedList = [Elem "a", Elem 1, Elem (Just 5)]
```
This is a heterogeneous list that contains variables of different types.
You couldn't do that without existential types, because even if you use type variables, you should still have every element in the list have the same type.

Note, that since type variable `a` is hidden with quantifier, you can't add constraints to it, beside those added to the constructor

```haskell
showFunc :: Show ??? => [Elem] -> String -- There is no type variable, that you can add constraint to
showFunc (x:xs) => (show x) ++ (showFunc xs)
```

The same "shadowing" is used in well-known `SomeException` wrapper:
```haskell
data SomeException = forall a. Exception a => SomeException a
```

It allows you to catch exception of any type (presumed it was wrapped inside `SomeException`) and then check if this is exception of the type you want.

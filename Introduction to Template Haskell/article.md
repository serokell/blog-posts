# A Brief Introduction to Template Haskell

In this article, we will explore some of the intricacies of Template Haskell and build a practical example that will introduce you to the metaprogramming capabilities of Haskell.

We will first use Template Haskell to generate some functions instead of writing them manually for the purpose of understanding how its basic machinery works. Then we will go through a more practical example and use TH to generate boilerplate code that will define class definitions and instances for those classes.

## What is Template Haskell?

_Template Haskell_, also known as _TH_, is a set of functions and datatypes exposed through the [template-haskell](https://hackage.haskell.org/package/template-haskell) package, which allows the programmer to manipulate Haskell code programmatically. Some of the things Template Haskell allows are:

* Generate new functions or datatypes procedurally.
* Inspect what will be generated for certain Haskell constructions.
* Execute code during compile-time.

Template Haskell is mostly useful for generating boilerplate code and automating some aspects of the compilation. In this article, we will focus on the former and use TH to generate boilerplate for us.

## Getting started

We are going to start by making a simple example that might be contrived but hopefully useful to understand how TH works. As the first step, let's see how we can generate basic functions with TH instead of writing them manually.

### Quoting and quasi-quoting

Template Haskell provides us with a simple tool to work with various parts of its machinery: quotations. With them, we can take a Haskell expression at compile-time and use it to create an Abstract Syntax Tree (or AST). They provide a shortcut for writing Template Haskell code in many situations and can also be used to help us inspect generated code.

Furthermore, Haskell provides a mechanism to extend the language with more quotations called quasi-quotations, in addition to the quotations in Template Haskell that are explored in this article, namely `e`, `p`, `t` and `d`.

It's important to understand some of the quoting mechanisms which can be used and what they produce. First, fire up GHCi with the `template-haskell` package and import the `Language.Haskell.TH` module, which is our main interest for this article. Note that we will also need to activate the `TemplateHaskell` language extension.

```hs
>>> import Language.Haskell.TH
>>> :set -XTemplateHaskell
```

Now we are ready for our first example. We will start by learning the `[e| ... |]` quoter, which takes in some Haskell expression and returns an AST representing that particular expression.

```hs
>>> runQ [e|1 + 2|]
InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))
```

By using `runQ :: Quasi m => Q a -> m a`, we can extract our quotation, which is then printed by GHCi. We will see later that `IO` and `Q` are two instances of the `Quasi` typeclass, and so we are able to use it from our interactive interpreter.

The result of this was an AST that represents our expression, which is better visualized when drawn as an actual tree.

```none
                InfixE
               /  |   \
           Just  VarE  Just
             /    |     \
         LitE  GHG.Num.+ LitE
           /               \
   IntegerL                 IntegerL
         /                   \
        1                     2
```

It's interesting to notice that `Just` has appeared in the operands. An expression such as `[e|(1 +)|]` would instead have `Nothing` as its second operand!

So far, we've been using quotation to write Template Haskell. But we also can use ordinary Haskell constructors defined in the `template-haskell` library. For example, instead of writing `[e|(4, 2)|]`, we may as well type out the full AST to obtain the same result:

```hs
>>> runQ (pure (TupE [Just (LitE (IntegerL 4)), Just (LitE (IntegerL 2))]))
TupE [Just (LitE (IntegerL 4)),Just (LitE (IntegerL 2))]
```

```none
               TupE
                |
                :
               / \
           Just   \
             /     :
         LitE     / \
           /  Just   []
   IntegerL     |
         /    LitE
        4       |
             IntegerL
                |
                2
```

Template Haskell follows a nice pattern of naming nodes for the AST where expressions will be suffixed with E, patterns with P, literals with L, declarations with D, and types with T. We also have quasi-quoters equivalent to some of the trees, namely `e`, `p`, `d`, and `t`.

As a matter of fact, working with `[e| ... |]` is so common that we can just use `[| ... |]` instead.

We've been using `e` to generate Haskell expressions so far, although it is also of interest to create other Haskell constructs, such as declarations. Without further ado, let's use Template Haskell to generate a declaration that is equivalent to the following Haskell code:

```hs
decl :: Int
decl = 1 + 2
```

Since we have a declaration, we may use the `d` quasi-quoter for this:

```hs
>>> runQ [d|
...         decl :: Int
...         decl = 1 + 2
...       |]
[ SigD decl_0 (ConT GHC.Types.Int)
, ValD (VarP decl_0) (
    NormalB (  -- NormalB is the body of a declaration without pattern guards
      InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))
    )
  ) []
]
```

The `template-haskell` package provides comprehensible documentation for all of the types generated by the [Exp](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH-Syntax.html#t:Exp), [Pat](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH-Syntax.html#t:Pat), [Dec](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH-Syntax.html#t:Dec) and [Type](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH-Syntax.html#t:Type) quasi-quoters.

### Splicing

So far, we've used TH to analyze the trees produced by various Haskell constructs. While that is interesting, it is not so useful since the main purpose of TH is to generate Haskell code that can be compiled and used. For this, we may use splices, which will essentially allow us to call our definitions made with TH.

```hs
-- A function which takes two other functions and composes them, as (.):
>>> compose :: Q Exp
... compose = [|\left right x -> left (right x)|]

>>> $compose (* 2) (+ 1) 0
2
```

What is going on with the `$compose`? This is a _splice_, and it allows us to use a given Template Haskell definition. The expression that immediately follows the `$` will evaluate the TH definition and return an actual Haskell definition that we can use. Splices will try to inject any kind of code that is given to them, so make sure it is correct. As demonstrated below, we can inject a bound variable in Template Haskell:

```hs
>>> x = 20
>>> compose' :: Q Exp
... compose' = [|\left right -> left (right x)|]
>>> $compose' (* 2) (+ 1)
42
```

Since Template Haskell is evaluated during compile time, if we replace `x` with a variable that was not bound, such as `z`, we get a `Variable not in scope: z` error instead during the splice generation. Any malformed expressions will be reported like any other regular Haskell expression. In other words, we analyze a Template Haskell declaration as if it was any other ordinary Haskell declaration.

### Stage restriction

One caveat of splices exists on where they may be defined and used within some source file. To visualize the problem, we will create a new file called `Main.hs` with the following:

```hs
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

someSplice :: Q [Dec]
someSplice = [d|y = 0|]

x :: Int
x = 42

$someSplice

z :: String
z = show x
```

Now try to load it in GHCi:

```
>>> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Failed, no modules loaded.
Main.hs:11:1: error:
    GHC stage restriction:
     ‘someSplice’ is used in a top-level splice, quasi-quote, or annotation,
      and must be imported, not defined locally
   |
11 | $someSplice
   | ^^^^^^^^^^^
```

Oops! We've encountered a stage restriction. If you ever come across a project that uses TH, you may notice that often there is a separate module for all things that are related to TH. To fix the error, let's move the definition of `someSplice` to a new `TH.hs` file.

```hs
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

someSplice :: Q [Dec]
someSplice = [d|y = 0|]
```

And change `Main.hs` to import it:

```hs
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import TH

x :: Int
x = 42

$someSplice

z :: String
z = show x
```

And now we should be able to load `Main` and use `y`.

Another restriction comes with declaration groups. Notably, if we swap where `x` and `z` are declared:

```hs
z :: String
z = show x

$someSplice

x :: Int
x = 42
```

and load the file again, we see a problem:

```
Main.hs:8:10: error:
    • Variable not in scope: x
    • ‘x’ (line 13) is not in scope before the splice on line 10
  |
8 | z = show x
  |
```

Whenever we insert a splice, the file is divided between everything that was declared before and after the splice. In our first example, `x` was in the scope of `z` but not the opposite, but after swapping both declarations, `z` is in the scope of `x` but not the opposite. To mitigate the problem, we often try to insert all splices at the very bottom of the file, if possible.

### -ddump-splices

It's useful to see what code was generated with Template Haskell. We can use the `-ddump-splices` to see the output:

```hs
>>> :set -ddump-splices
```

If we load `Main` again (this time with the correct order of declarations), we should see the generated output of the splice:

```h
>>> :r
[2 of 2] Compiling Main             ( Main.hs, interpreted )
Main.hs:10:1-11: Splicing declarations
    someSplice ======> y_a65Q = 0
Ok, two modules loaded.
```

The output shows our `y` was declared as `0`.

## Example: generating instances

Let's create a simple example to best understand how TH works. One thing that could be potentially useful is a way to extract the third, fourth, fifth, etc. element of some tuple. Users of the `lens` library probably have found methods such as `_1`, `_2`, `_3`, etc. to deal with tuples of arbitrary sizes. Let's try to generate these instances automatically using TH.

We will use the following approach. First, we will define typeclasses `Tuple2`, `Tuple3`, ... for tuples of arbitrary sizes. Each of them will have methods `_1`, `_2`, `_3`, ... for accessing elements of tuples. After this, we will generate instances for each `(a, b)`, `(a, b, c)`, `(a, b, c, d)`, etc.

To begin, let's start by creating a file `TuplesTH.hs`:

```hs
{-# LANGUAGE TemplateHaskell #-}

module TuplesTH where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH
```

How does TH represent the tuple of a tuple as an AST? We could [check the documentation](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH.html#t:Type), but we can also check out GHCi:

```hs
>>> runQ [d|x :: (a,b,c); x = undefined|]
[ SigD x_8 (AppT (AppT (AppT (TupleT 3) (VarT a_5)) (VarT b_6)) (VarT c_7))
, ValD (VarP x_8) (NormalB (VarE GHC.Err.undefined)) []
]
```

Don't worry if your variable names are not the same. The relevant part for us is `AppT (AppT (AppT (TupleT 3) (VarT a_5)) (VarT b_6)) (VarT c_7)`:

```none
                    AppT
                   /    \
               AppT      VarT
              /    \      \
          AppT      VarT   c_7
         /    \      \
   TupleT      VarT   b_6
       /        \
      3          a_5
```

We must mimic this structure for our class instance declarations. Not only this, but our method will have the format `(a, b, ..., t, ...) -> t` where `t` is the n-th type of our tuple. Again, let's see what is the Template Haskell equivalent for a function:

```hs
>>> runQ [d|x :: a -> b; x = undefined|]
[ SigD x_15 (AppT (AppT ArrowT (VarT a_13)) (VarT b_14))
, ValD (VarP x_15) (NormalB (VarE GHC.Err.undefined)) []
]
```

We see that an arrow `(->)` is represented as `ArrowT`. Keep in mind that `a -> b` is the same as `(->) a b`.

```none
                AppT
               /    \
           AppT      VarT
          /    \      \
    ArrowT      VarT   b_14
                 \
                  a_13
```

With this, we can create a function which generates a tuple class. We have a declaration, so by looking up [the documentation for `Dec`](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH-Syntax.html#t:Dec) we find our constructor for declaring a class: `ClassD Cxt Name [TyVarBndr ()] [FunDep] [Dec]`. This class should have the form `class TupleX t r | t -> r where _X :: t -> r`, where `X` is the number of elements in the tuple. With this information, we can declare a function which will generate such class for us:

```hs
generateTupleClass :: Int -> Q [Dec]
generateTupleClass size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ size'
  pure [cDecl]
  where
    size' = show size
    className = mkName ("Tuple" ++ size')
    methodName = mkName ('_' : size')

    t = mkName "t"
    r = mkName "r"

    -- class TupleX t r | t -> r where
    cDecl = ClassD [] className [PlainTV t, PlainTV r] [FunDep [t] [r]] [mDecl]
    --   _X :: t -> r
    mDecl = SigD methodName (AppT (AppT ArrowT (VarT t)) (VarT r))
```

We only handle cases with at least 1 element, so we throw an error if we have less than this. We use the `mkName` function to create the names of our class (`TupleX`), its method (`_X`), and the `t` and `r` type variables.

For the declaration of the class, we use the `ClassD` constructor with the following fields:

* `[] :: Ctx` — An empty array of constraints, since we don't impose further restrictions on our types.
* `className :: Name` — The name of the class being declared.
* `[PlainTV t, PlainTV r] :: [TyVarBndr ()]` — The bindings of our typeclass, which are `t` and `r`.
* `[FunDep [t] [r]] :: [FunDep]` — A functional dependency of the format `t -> r`.
* `[mDecl] :: [Dec]` — Our class declarations, containing `_X`, a method of type `t -> r`.

Now let's create a `Main.hs` and test out what we made:

```hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import TuplesTH

$(generateTupleClass 3)
```

If we now load this in GHCi, we get:

```hs
>>> :l Main
[2 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, two modules loaded.

>>> :i Tuple3
type Tuple3 :: * -> * -> Constraint
class Tuple3 t r | t -> r where
  _3 :: t -> r
  {-# MINIMAL _3 #-}
        -- Defined at Main.hs:8:3
```

Hooray! We made a class declaration using TH. Now let's generate some instances. In the same manner as `ClassD`, we can use `InstanceD` for this, which in `Dec` has a constructor of the form `InstanceD (Maybe Overlap) Ctx Type [Dec]`. What should the AST for the instance look like? Let GHCi come to our rescue again!

```hs
>>> runQ [d|instance Tuple3 (a, b, c) c where _3 (_, _, c) = c|]
[InstanceD
  Nothing
  []
  (AppT (AppT (ConT Main.Tuple3) (AppT (AppT (AppT (TupleT 3) (VarT a_4)) (VarT b_5)) (VarT c_6))) (VarT c_6))
  [FunD
    Main._3 [Clause [TupP [WildP,WildP,VarP c_7]] (NormalB (VarE c_7)) []]]]
```

This is a bit verbose, but we can notice two useful things. The first one is that we have a nested sequence of `AppT` for our tuple type like before. What's interesting right now is how the tuple is represented as a sequence of `AppT other_app_ts some_type_var`, with `TupleT 3` at the end. We can use `foldl` to apply the `AppT` nodes to some `VarT` variable, using `TupleT 3` as the initial value for our fold. After this, we only need to apply the result of the fold inside `AppT (AppT (ConT our_class_name) result_of_our_fold)`.

The second thing to notice is that `InstanceD` also expects a declaration, in our case a function, represented as `FunD`. We want a pattern for our tuple where the X-th tuple element has a variable pattern and the other ones are wildcards. We can create a function that takes the index of the element we want to generate a getter for and the total size of the tuple and generates an instance accessing this element.

Having this in mind, we can create our tuple type `(t1, t2, ...)` and generate a type `TupleX (t1, t2, ...) tX` which is the instance declaration for our class with its corresponding method declaration, which is a function `_X (_, _, ..., x, ...) = x` that corresponds to our accessor.

```hs
generateTupleInstance :: Int -> Int -> Q [Dec]
generateTupleInstance element size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ element'
  unless (size >= element) $
    fail $ "Can't extract element " ++ element' ++ " of " ++ size' ++ "-tuple"
  pure [iDecl]
  where
    element' = show element
    size' = show size
    className = mkName ("Tuple" ++ element')
    methodName = mkName ('_' : element')

    x = mkName "x"

    vars = [mkName ('t' : show n) | n <- [1..size]]

    signature = foldl (\acc var -> AppT acc (VarT var)) (TupleT size) vars

    -- instance TupleX (t1, ..., tX, ...) tX where
    iDecl = InstanceD Nothing [] (AppT (AppT (ConT className) signature) (VarT $ mkName ('t' : element'))) [mDecl]
    --   _X (_, _, ..., x, ...) = x
    mDecl = FunD methodName [Clause [TupP $ replicate (element - 1) WildP ++ [VarP x] ++ replicate (size - element) WildP] (NormalB $ VarE x) []]
```

If we now add `$(generateTupleInstance 3 5)` to our `Main` module and load it in GHCi, we can see that this indeed generated an instance:

```hs
>>> :i Tuple3
type Tuple3 :: * -> * -> Constraint
class Tuple3 t r | t -> r where
  _3 :: t -> r
  {-# MINIMAL _3 #-}
        -- Defined at Main.hs:8:3
instance Tuple3 (t1, t2, t3, t4, t5) t3 -- Defined at Main.hs:9:3

>>> _3 (42, "hello", '#', [], 3.14)
'#'
```

Finally, let's add one more TH function which will generate instances for all tuples up to 62 elements (which is the maximum tuple size allowed by GHC). We want to generate N instances for all M-tuples such that N ≤ M:

```hs
generateTupleBoilerplate :: Int -> Q [Dec]
generateTupleBoilerplate size =
  concatFor [1..size] $ \classDeclIndex -> do
    cDecl <- generateTupleClass classDeclIndex
    iDecls <- for [1..classDeclIndex] $ \instanceDeclIndex ->
      generateTupleInstance instanceDeclIndex classDeclIndex

    pure $ concat (cDecl : iDecls)
  where
    concatFor xs = fmap concat . for xs
```

Now change your `Main` to contain `$(generateTupleBoilerplate 62)` and voilá! We have all instances for tuples.

### Exercises

1. Change the definition of `TupleX` so that each class declaration also contains a `Tuple(X-1)` constraint. For example, `class Tuple2 a => Tuple3 a`, `class Tuple1 a => Tuple2 a` etc. Notice `Tuple1` should have no constraints.
2. Create a `mapX` method that maps the X-th element of a tuple, similarly to `fmap`.

## Further reading

In this post, we've seen the basics of Template Haskell. We used it to declare some instances, inspect the generated AST, and implement instances for tuple types. For more resources on Template Haskell, I suggest checking out the following links:

* [Template Haskell tutorial](https://markkarpov.com/tutorial/th.html)
* [Compile-Time Evaluation](https://serokell.io/blog/compile-time-evaluation-haskell)

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Medium](https://serokell.medium.com/).

Welcome to our second post on Template Haskell!

Today we will take a quick look at typed Template Haskell. This article assumes some familiarity with Template Haskell (TH) already. If this is your first journey with TH, then check out our [introduction to Template Haskell](https://serokell.io/blog/introduction-to-template-haskell) first.

For this article, we will be using GHC 8.10.4.

## Why typed TH?

Typed TH, as the name implies, allows us to provide stronger, static guarantees about the correctness of the meta-program. With untyped TH, the generated expressions would be type-checked when they are spliced, i.e., during their usage, rather than their definition. With typed TH, those expressions are now type-checked at their definition site.

Like with anything else in computer science, there are advantages and disadvantages in using typed TH in comparison to ordinary TH, some of which are listed below.

**Advantages:**
* Greater type safety guarantees.
* Errors won't be delayed until use; instead, they are reported on their definition.

**Disadvantages:**
* Must be used with the `[|| ||]` quoter.
    * This means that we can't easily use `Exp` constructors directly.
    * In comparison, for the untyped quoter, we could either use `[| |]` or directly call the `Exp` constructors.
    * Alternatively, you may use [`unsafeCodeCoerce`](https://hackage.haskell.org/package/template-haskell-2.17.0.0/docs/Language-Haskell-TH.html#v:unsafeCodeCoerce) to work around this, if you're willing to use unsafe functions.
* Only supports a typed version of `Exp` (no typed version for `Dec`, `Pat`, etc).
    * Our previous TH tutorial could not have been written purely with Typed TH, as it heavily uses `Dec`, for example.
* Requires that the type being used is known in advance, which may limit the kinds of TH programs you can make.

<hr>

Before we begin, make sure you have the [`template-haskell`](https://hackage.haskell.org/package/template-haskell) package installed, as well as the `TemplateHaskell` language extension enabled.

```hs
>>> :set -XTemplateHaskell
>>> import Language.Haskell.TH
```
## Typed expressions

In our previous tutorial, we learned that we could use the `[e|...|]` quoter (which is the same as `[|...|]`) to create expressions of type `Q Exp`. With typed TH, we will use `[e||...||]` (which is the same as `[||...||]`) to create expressions of type `Q (TExp a)`.

What is `TExp a`, you might wonder? It's simply a `newtype` wrapper around our familiar `Exp`:

```hs
type role TExp nominal
newtype TExp (a :: TYPE (r :: RuntimeRep)) = TExp
  { unType :: Exp
  }
```

The meaning of the `TYPE (r :: RuntimeRep)` part is not important to us, but simply put, it allows GHC to describe how to represent some types (boxed, unboxed, etc) during runtime. For more information, see [levity polymorphism](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/#levity-polymorphism).

This allows us to use our familiar constructions for `Exp`, in addition to a type for `a` which represents the type of the expression. This gives us stronger type-safety mechanisms for our TH application, which will cause the compiler to reject invalid TH programs during their construction.

In the example below, `template-haskell` gladly accepts `42 :: String` using an untyped expression, while the typed counterpart refuses it with a type error.

```hs
>>> runQ [|42 :: String|]
SigE (LitE (IntegerL 42)) (ConT GHC.Base.String)

>>> runQ [||42 :: String||]
<interactive>:358:9: error:
    • Could not deduce (Num String) arising from the literal ‘42’
      from the context: Language.Haskell.TH.Syntax.Quasi m
        bound by the inferred type of
                   it :: Language.Haskell.TH.Syntax.Quasi m => m (TExp String)
        at <interactive>:358:1-23
    • In the Template Haskell quotation [|| 42 :: String ||]
      In the first argument of ‘runQ’, namely ‘[|| 42 :: String ||]’
      In the expression: runQ [|| 42 :: String ||]
```

## Typed splices

Just like we had untyped splices such as `$foo`, now we also have typed splices, written as `$$foo`. In this section, we will see how to define and splice typed Template Haskell code.

We reiterate one point mentioned in our previous article: in GHC 8, the usage of splices may require parentheses or not. For instance, if `foo` came from a qualified import, then you'd need to write `$$(Foo.foo)` instead. In GHC 9, the parser is more relaxed and will not require parentheses in such situations.

## Example: calculating prime numbers

As an example, let's consider the following functions that implement prime number evaluation up to some number. We will make create two versions, one with ordinary Haskell, and another with Template Haskell, so we can see the differences between them. The implementation may be somewhat more verbose than it needs to be to demonstrate the techniques in typed TH and contrast them with an ordinary function.

First, create a file `Primes.hs` containing two functions: one that checks whether a given a number is prime, and another that generates primes numbers up until some given limit.

```hs
module Primes where

isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False  -- No even number except for 2 is prime
  | otherwise = go 3
  where
    go i
      | i >= n         = True  -- We saw all smaller numbers and no divisors, so it's prime
      | n `mod` i == 0 = False
      | otherwise      = go (i + 2)  -- Iterate through the odd numbers

primesUpTo :: Integer -> [Integer]
primesUpTo n = go 2
  where
    go i
      | i > n     = []
      | isPrime i = i : go (i + 1)
      | otherwise = go (i + 1)
```

The first function checks whether a number has any divisors. If it has any divisor (apart from 1 and itself), then the number is composite and the function returns `False`, otherwise it keeps testing for more divisors. If we reach a number that is greater or equal to the input, it means that we have checked all smaller numbers and found no divisors, and so the number is prime, and the function returns `True`.

The second function simply iterates through the numbers, collecting all primes. We start with 2 since it's the first prime number.

Keep in mind that these functions are _very_ inefficient, so make sure to use a more optimized version for anything serious!

Now for our Template Haskell version. As usual, let's create two files, `TH.hs` and `Main.hs`, to work with through this example.

This is what should be in `TH.hs`:
```hs
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

import Primes (isPrime)

primesUpTo' :: Integer -> Q (TExp [Integer])
primesUpTo' n = go 2
  where
    go i
      | i > n     = [||[]||]
      | isPrime i = [||i : $$(go (i + 1))||]
      | otherwise = [||$$(go (i + 1))||]
```

In general, it's the same thing as the ordinary version. The only difference now being that we return a `Q (TExp [Integer])` and generate our list inside the typed expression quoter.

We wrap our recursive calls to `go` inside splices. Since `go` has a type of `Q (TExp [Integer])`, if we didn't splice it, we'd try to use the cons operator (`:`) on an `Integer` and a `Q (TExp [Integer])` which would not type-check. An error message might describe the problem quite well:

```hs
>>> :l TH
[2 of 2] Compiling TH               ( TH.hs, interpreted )
Failed, no modules loaded.
TH.hs:15:21: error:
    • Couldn't match type ‘Q (TExp [Integer])’ with ‘[Integer]’
      Expected type: Q (TExp [Integer])
        Actual type: Q (TExp (Q (TExp [Integer])))
    • In the Template Haskell quotation [|| (go (i + 1)) ||]
      In the expression: [|| (go (i + 1)) ||]
      In an equation for ‘go’:
          go i
            | i > n = [|| [] ||]
            | isPrime i = [|| i : $$(go (i + 1)) ||]
            | otherwise = [|| (go (i + 1)) ||]
   |
15 |       | otherwise = [||(go (i + 1))||]
   |                     ^^^^^^^^^^^^^^^^^^
```

As a matter of fact, we could have written that branch above simply as `go (i + 1)`, without the quoter. Try it!

And now we can use our new function in GHCi like so:

```hs
>>> $$(primesUpTo' 100)
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

We can also inspect it as an untyped Template Haskell definition if we want, using the `unType` function:

```hs
>>> runQ (unType <$> primesUpTo' 10)
InfixE (Just (LitE (IntegerL 2))) (ConE GHC.Types.:) (Just (InfixE (Just (LitE (IntegerL 3))) (ConE GHC.Types.:) (Just (InfixE (Just (LitE (IntegerL 5))) (ConE GHC.Types.:) (Just (InfixE (Just (LitE (IntegerL 7))) (ConE GHC.Types.:) (Just (ConE GHC.Types.[]))))))))
```

Or, more simply put:

```hs
2 : 3 : 5 : 7 : []
```

Had we made any mistakes in the definition, for example, by using the following definition where we forget a recursive call:

```hs
primesUpTo' :: Integer -> Q (TExp [Integer])
primesUpTo' n = go 2
  where
    go i
      | i > n     = [||[]||]
      | isPrime i = [||i||]  -- We forgot to build a list here
      | otherwise = go (i + 1))
```

Then we'd be immediately greeted with a type-error:

```hs
>>> :r
[2 of 2] Compiling TH               ( TH.hs, interpreted )
Failed, no modules loaded.
TH.hs:14:21: error:
    • Couldn't match type ‘Integer’ with ‘[a]’
      Expected type: Q (TExp [a])
        Actual type: Q (TExp Integer)
    • In the Template Haskell quotation [|| i ||]
      In the expression: [|| i ||]
      In an equation for ‘go’:
          go i
            | i > n = [|| [] ||]
            | isPrime i = [|| i ||]
            | otherwise = [|| $$(go (i + 1)) ||]
    • Relevant bindings include
        go :: Integer -> Q (TExp [a]) (bound at TH.hs:43:5)
   |
14 |       | isPrime i = [||i||]
   |                     ^^^^^^^
```

Our `primesUpTo'` will generate the list of primes at compile-time, and now we can use this list to check the values at runtime.

With this, we can create our `Main.hs`, where we can try our code:
```hs
{-# LANGUAGE TemplateHaskell #-}

import TH

main :: IO ()
main = do
  let numbers = $$(primesUpTo' 10000)
  putStrLn "Which prime number do you want to know?"
  input <- readLn  -- n.b.: partial function
  if input < length numbers
    then print (numbers !! (input - 1))
    else putStrLn "Number too big!"
```

And that's it! A very simple program using typed TH. Load `Main.hs` in GHCi, and after a few seconds when it's loaded, run our `main` function. Once asked for an input, type a number such as 200, asking for the 200th prime. The function should output the [correct result of 1223](http://allthingsuniverse.com/prime/number/1223.html).

```hs
>>> main
Which prime number do you want to know?
200
1223
```

Again, our algorithm is quite inefficient and this may take some seconds to compile (as it's generating numbers as it compiles), and for further improvements, it may be a good idea to have a less naïve algorithm for generating primes, but for educational purposes, it will do for now.

The code used in this post can also be found in [this GitHub gist](https://gist.github.com/heitor-lassarote/dc862c40c9a7fb9b7693fa1467d61605).

### A shorter implementation

As mentioned before, we could implement the functions above in a more simple manner, such as:

```hs
primesUpTo :: Integer -> [Integer]
primesUpTo n = filter isPrime [2 .. n]
```

And the corresponding TH function as:

```hs
primesUpTo' :: Integer -> Q (TExp [Integer])
primesUpTo' n = [|| primesUpTo n ||]
```

And with this, you should be ready to use typed Template Haskell in the wild.

## Caveat

Typed Template Haskell may have some difficulties resolving overloads. Surprisingly, the following does not type-check:

```hs
>>> mempty' :: Monoid a => Q (TExp a)
... mempty' = [|| mempty ||]

>>> x :: String
... x = id $$mempty'
<interactive>:549:11: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘mempty'’
      prevents the constraint ‘(Monoid a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
        instance Monoid Ordering -- Defined in ‘GHC.Base’
        instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
        ...plus 7 others
        (use -fprint-potential-instances to see them all)
    • In the expression: mempty'
      In the Template Haskell splice $$mempty'
      In the first argument of ‘id’, namely ‘$$mempty'’
```

Annotating `mempty'` may resolve it in this case:

```hs
>>> x :: String
... x = id $$(mempty' :: Q (TExp String))

>>> x
""
```

An [open ticket](https://gitlab.haskell.org/ghc/ghc/-/issues/10271) exists describing the issue, but if you run into some strange errors, it's a good idea to keep it in mind.

## Further reading

In this post, we extended our Template Haskell knowledge with `TExp`. We created a short example where we generated some values during compile time that can be later looked up at runtime. For more resources on typed Template Haskell, check out the following links:

* [Using Template Haskell to generated static data](https://well-typed.com/blog/2020/06/th-for-static-data/)
* [A Little Bloop on Typed Template Haskell](https://www.philipzucker.com/a-little-bloop-on-typed-template-haskell/)
* [Statically checked overloaded string](https://gist.github.com/chrisdone/809296b769ee36d352ae4f8dbe89a364)

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Medium](https://serokell.medium.com/).

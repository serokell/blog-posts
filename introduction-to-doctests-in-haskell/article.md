# Introduction to Doctests in Haskell

Documenting software can be challenging, but it doesn't always need to be so.

In this article, we'll introduce doctests: a concept that makes the documentation process pleasant and effective.

By putting tests inside module docs, doctests allow functions 
to speak for themselves and help you get the most out of your testing efforts.

Read the article to learn:

* what doctests are; 
* how to define them in Haskell;
* which library to use for Haskell doctests.

## What are doctests?

Doctests are simply pieces of text embedded in the documentation that look like interactive sessions. 
With a special library, you can run these sessions and verify that they return the correct value.

The idea for this comes from Python's awesome 
[`doctest`](https://en.wikipedia.org/wiki/Doctest) module, but since then has spread
to virtually every programming language.

In Haskell, doctests are GHCi sessions within [Haddock comments](https://www.haskell.org/haddock/).

If you ever had to skim through the Haskell source code documentation, you might have noticed those lines with fancy `>>>` symbols at the beginning.

```haskell
-- | @const x@ is a unary function which evaluates to @x@ for all inputs.
--
-- >>> const 42 "hello"
-- 42
--
-- >>> map (const 42) [0..3]
-- [42,42,42,42]
```

As you might have guessed, those are doctests.

## How to define a doctest in Haskell?

Let's look at an example of a basic doctest. 

```haskell
-- | 1 + 2 is 3.                          
-- >>> 1 + 2                 
-- 3
```

As you can see, Haskell doctests have three requirements:

* Every doctest example should be placed within a valid piece of [Haddock documentation](https://haskell-haddock.readthedocs.io/en/latest/markup.html), which is marked by either `-- |` or `{- |`.

* Every doctest example should start with `>>>` and contain a valid Haskell expression that is in scope (sometimes you have to be explicit about the `import` statements – we'll cover that further in the article).

* Every doctest example should be followed by a line containing the expected result of evaluating the expression.

You can find additional info on doctest markup in the [readme](https://github.com/sol/doctest#writing-examples-and-properties) of the `doctest` library.

Now, let's create a project with doctests and go over Haskell libraries that you can use to run those doctests.

## Creating a Haskell project with doctests

To work with doctest libraries, we need to create a Haskell project with doctests. 

First, bootstrap a project using [`stack`](https://docs.haskellstack.org/en/stable/README/):

```
stack new doctests-demo
```

After that, go to the project's root directory and create a [Haskell module](https://www.haskell.org/tutorial/modules.html):

```
cd doctests-demo
```

```
touch src/Sample.hs
```

Finally, add some functions with doctests to the newly created `Sample.hs` module:

```haskell
-- src/Sample.hs

module Sample where

-- |
-- >>> foo + 13
-- 55
foo :: Integer
foo = 42

-- |
-- >>> bar
-- "bar"
bar :: String
bar = "bar"

```

The project is now ready for running doctests.

## Doctest libraries in Haskell

We'll cover two doctest libraries in the Haskell ecosystem: [doctest](https://github.com/sol/doctest) and [`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md). The first one is older and more popular, the second one is less popular but solves some of `doctest`'s issues. 

### `doctest`

[doctest](https://github.com/sol/doctest) is one of the most commonly used and actively maintained doctest libraries.
At the same time, it has drawbacks like bad performance for large-scale projects and dependency on GHC as a library. 

#### How to use `doctest`

First, install the library via `stack`:

```
stack install doctest
```

After that, go to the project's root directory and use the library's executable:

```
cd doctests-demo
```

```
doctest src
```

The command above should output something like this:

```
Examples: 2  Tried: 2  Errors: 0  Failures: 0
```

#### Cons of using `doctest`

While it's an awesome piece of software, the library has some drawbacks.

The biggest drawback is that the library seems to be too slow for large-scale projects.
One of the reasons causing performance issues is that the library reloads the source between each group of doctest examples.
This is done to avoid example groups influencing each other.
You can learn more about this in the [library's readme](https://github.com/sol/doctest/blob/main/README.md#a-note-on-performance).

There are also several more minor drawbacks.

First, it has a dependency on [GHC as a library](https://wiki.haskell.org/GHC/As_a_library), which means it's very likely that something will eventually 
break down when you switch the project to a new compiler version.

Second, when used via [`stack`](https://docs.haskellstack.org/en/stable/README/), you might need to create an additional test suite to bring the project
dependencies into scope.

<details><summary>Illustration of the dependency drawback.</summary>

Let's illustrate this by editing our doctest examples a little (don't forget to add `aeson` and `text` to the list of dependencies):

```haskell
-- src/Sample.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Sample where

import Data.Aeson
import Data.Aeson.TH
import Data.Text

data Anime =
    Anime { title  :: Text
          , rating :: Double
          }

$(deriveJSON defaultOptions ''Anime)

-- |
-- >>> encode favourite
-- "{\"title\":\"One-Punch Man\",\"rating\":8.9}"
--
favourite :: Anime
favourite = Anime "One-Punch Man" 8.9
```

Now, if we try to run the doctests using `doctest src`, we'll fail with the following message:

```
src/Sample.hs:9:1: error:
    Could not find module ‘Data.Aeson’
    Perhaps you meant Data.Version (from base-4.14.3.0)
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
    |
9 | import Data.Aeson
    | ^^^^^^^^^^^^^^^^^
```

The workaround is to create an additional test suite.

```
tests:

    doctests:
    source-dirs: doctests
    main: Main.hs
    ghc-options:
        - -threaded
        - -rtsopts
        - -with-rtsopts=-N
    dependencies:
        - doctest
        # bring in your project into the scope
        # as well as its dependencies
        - doctests-demo
```

After that, you need to add the test suite directory and `Main.hs` executable:

```
mkdir doctests
```

```
touch doctests/Main.hs
```

```haskell
-- doctests/Main.hs

import           Test.DocTest

-- This test suite exists only to add dependencies
main :: IO ()
main = doctest ["src"]
```

Finally, run the doctests using the following command:

```
stack test :doctests
```   
<hr>
 
</details>
<br>

### `cabal-docspec`

[`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md) has fewer contributors
and less overall community attention, but a bunch of people already use it in real projects.

#### How to use `cabal-docspec` 

Let's use `cabal-docspec` to run the doctests in our `doctests-demo` project.

First, setup `cabal-install` and the global compiler.
We suggest using [ghcup](https://www.haskell.org/ghcup/) to achieve this.

After that, build the project with Cabal:

```
cabal v2-build
```

Then download `cabal-docspec` binaries from the [release page](https://github.com/phadej/cabal-extras/releases):

```
curl -sL https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20211114/cabal-docspec-0.0.0.20211114.xz > cabal-docspec.xz
xz -d < cabal-docspec.xz > "$HOME"/.local/bin/cabal-docspec
rm -f cabal-docspec.xz
chmod a+x "$HOME"/.local/bin/cabal-docspec
```

Now, run the doctest examples:

```
cabal-docspec
```

The command above should fail with the following error:

```
expected: "{\"title\":\"One-Punch Man\",\"rating\":8.9}"
but got:
            ^
            <interactive>:10:1: error:
                Variable not in scope: encode :: Anime -> t
```

The above happens because the library handles things a little differently – it requires modules to be explicitly imported/exported.

Let's edit our doctests a little to make `cabal-docspec` work:

```diff
 -- |
+ -- >>> import Data.Aeson
 -- >>> encode favourite
 -- "{\"title\":\"One-Punch Man\",\"rating\":8.9}"
 --
 favourite :: Anime
 favourite = Anime "One-Punch Man" 8.9

```

Now the `cabal-docspec` command should succeed:

```none
Total:         2; Tried:    2; Skipped:    0; Success:    2; Errors:    0; Failures    0
Examples:      2; Tried:    2; Skipped:    0; Success:    2; Errors:    0; Failures    0
```

#### Why you should use `cabal-docspec`

Here are the reasons why you might want to use `cabal-docspec` for your project: 

* The library is a lot faster than the alternatives because it uses compiled code.

* The library doesn't depend on GHC as a library, so it's much more resilient to GHC version changes.

* The library doesn't require recompiling the source code if you only change the doctests – this saves additional time for developers and
makes writing documentation pleasant.

All of the above makes it more than a viable alternative.

However, note that the library seems to be closely bound to [`cabal-install`](https://wiki.haskell.org/Cabal) since it uses `cabal-install` generated metadata – the `plan.json` file.

## Conclusions 

Thanks for reading! 

In the article, we looked at the concept of doctests, learned how to define them in Haskell, 
and gave a brief overview of two libraries that can help us verify them: `doctest` and `cabal-docspec`.

In the next part of this series, we'll cover the configuration and internals of `cabal-docspec` in more detail. 
To stay updated, follow us on [Twitter](https://twitter.com/serokell) or subscribe to the newsletter via the form below. 

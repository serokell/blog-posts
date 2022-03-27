# Introduction to Doctests in Haskell

If you ever had to skim through the Haskell source code documentation, you might have noticed those lines with fancy `>>>` symbols at the beginning.
They are called 'doctests' or 'doctest examples'.

To put it simply, doctests are pieces of text within [Haddock comments](https://www.haskell.org/haddock/) that look like interactive Haskell sessions.
The idea to search for and execute those sessions all over Haskell modules actually comes from the awesome Python's
[`doctest`](https://en.wikipedia.org/wiki/Doctest) module.

Documenting software is extremely challenging.
Doctests make the documentation process not only pleasant and effective – it actually allows Haskell functions to speak for themselves – but
also helps to get the most out of your testing efforts.

## How to define a doctest?


Let's look at an example of a doctest. 

```haskell
-- |
-- >>> 1 + 2
-- 3
```

```haskell
-- |
-- >>> putStrLn ""
-- <BLANKLINE>
```

These are some basic markup requirements:

* Every doctest example should be placed within a valid piece of Haddock documentation, that is followed by either special `-- |` or `{- |` syntaxes.

* Every doctest example should start with `>>>` and contain a valid Haskell expression that is in scope (sometimes you have to be explicit about the `import` statements- we'll cover that further in the article).

* Every doctest example is evaluated in a usual [GHCi](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) session. Therefore, it should be followed by a line containing the expected evaluation result.


You can find the rest of the markup rules, best detailed and covered, [where it all started](https://github.com/sol/doctest#writing-examples-and-properties).

Next, we'll create a demo project and go over libraries that you can use to run the doctest examples.
We'll pay special attention to the brand new [`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md)
library.

## Creating a demo project containing doctests

To illustrate doctests clearly, we need a Haskell project to use them in.
So let's create one with short and concise doctest examples.

After that, we'll launch these examples using [doctest](https://github.com/sol/doctest) and [`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md) libraries in the following sections.

1. Bootstrap a project using [`stack`](https://docs.haskellstack.org/en/stable/README/) first:

`stack new doctests-demo`

2. Go to the project's root directory and create a [Haskell module](https://www.haskell.org/tutorial/modules.html):

`cd doctests-demo`

`touch src/Sample.hs`

3. Add some dummy functions with doctests to the newly created `Sample.hs` module:

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

## Doctest libraries in Haskell

### `doctest`

[doctest](https://github.com/sol/doctest) is one of the most commonly used and actively maintained libraries.

To launch our doctest examples with it, follow the steps below:

1. Install the library via `stack`:

`stack install doctest`

2. Go to the project's root directory and use the library's executable:

`cd doctests-demo`

`doctest src`

3. The command above should output something like the following:

`Examples: 2  Tried: 2  Errors: 0  Failures: 0`

#### Cons of using `doctest`

While it's an awesome piece of software, the library has some drawbacks.

The biggest drawback is that the library seems to be slow enough for large-scale projects.
One of the reasons causing performance issues is that the library reloads the sources between each doctests' example group.
This is done to avoid those example groups depending on each other.
Using the `--fast` flag helps the situation a little, but this is not an option anymore with recent GHC versions.
You can learn more about this in the [library's readme](https://github.com/sol/doctest/blob/main/README.md).

There also several more minor drawbacks.

First, it has a dependency on [GHC as a library](https://wiki.haskell.org/GHC/As_a_library), which means it's very likely that something will eventually 
break down when you switch the project to a new compiler version.

Second, when used via [`stack`](https://docs.haskellstack.org/en/stable/README/), you might need to create an additional test suite to bring project
dependencies into the scope.

Let's illustrate this by editing our doctest examples a little (don't forget to add `aeson` and `text` packages to the list of dependencies):

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

`mkdir doctests`

`touch doctests/Main.hs`

```haskell

-- doctests/Main.hs

import           Test.DocTest

-- This test suite exists only to add dependencies
main :: IO ()
main = doctest ["src"]
```

Finally, run the doctests using the following command:

`stack test :doctests`

### `cabal-docspec`

[`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md) library has fewer contributors
and less overall community attention, but there already are a bunch of people using it in real projects.

#### Pros of using `cabal-docspec`

Here are the reasons why you might want to use `cabal-docspec` for your project. 

* The library is a lot faster than the alternatives because it uses the compiled code.

* The library doesn't depend on GHC as a library, so it's much more resilient to GHC version changes.

* The library doesn't require recompiling the source code if it's only about writing doctests – this saves additional time for developers and
makes writing documentation pleasant.

All of the above makes it more than a viable alternative.

However, the library seems to be closely bound to [`cabal-install`](https://wiki.haskell.org/Cabal) since it uses `cabal-install` generated metadata – the `plan.json` file.

#### How to use `cabal-docspec` 

Let's use `cabal-docspec` to run the doctests in our `doctests-demo` project.

First, setup `cabal-install` and the global compiler.
We suggest using [ghcup](https://www.haskell.org/ghcup/) to achieve this.

Build the project using `cabal`:

`cabal v2-build`

Download `cabal-docspec` binaries from the [release page](https://github.com/phadej/cabal-extras/releases).
You can also consider building the library from source, following the instructions [here](https://github.com/phadej/cabal-extras).

```
curl -sL https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20211114/cabal-docspec-0.0.0.20211114.xz > cabal-docspec.xz
xz -d < cabal-docspec.xz > "$HOME"/.local/bin/cabal-docspec
rm -f cabal-docspec.xz
chmod a+x "$HOME"/.local/bin/cabal-docspec
```

Now, run the doctest examples using `cabal-docspec`:

`cabal-docspec`

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

Now, the `cabal-docspec` command should succeed:

```none
Total:         2; Tried:    2; Skipped:    0; Success:    2; Errors:    0; Failures    0
Examples:      2; Tried:    2; Skipped:    0; Success:    2; Errors:    0; Failures    0
```

<hr>

Thanks for reading!

In the next part of this series, we will post about the configuration and internals of `cabal-docspec`. To keep updated, follow us on [Twitter](https://twitter.com/serokell) or subscribe to the newsletter via the form below. 



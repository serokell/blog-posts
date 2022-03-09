# Introduction to Doctests in Haskell

### What are doctests?

To put it simply, doctests are pieces of text within [haddock comments](https://www.haskell.org/haddock/) that look like interactive Haskell sessions.
The idea to search for and execute those sessions all over Haskell modules actually comes from the awesome
[`doctest`](https://en.wikipedia.org/wiki/Doctest) module, which is included in the [`Python programming language`](https://en.wikipedia.org/wiki/Python_(programming_language)).

Documenting software is extremely challenging.
The doctests concept makes the documentation process not only pleasant and effective - it actually allows Haskell functions to speak for themselves - but
also helps to get the most out of your testing efforts.

For the sake of simplicity and convenience, we won't cover the markup used to define a doctest.
Instead, we'll try to go over various libraries the community uses to run the doctest examples.
Particularly, we'll describe a brand new [`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md)
library in a little bit more details.

### Creating a demo project containing doctests

The doctests concept can be best illustrated within [a haskell project](https://wiki.haskell.org/How_to_write_a_Haskell_program).
So let's create one with short and concise doctest examples.
Then we'll be launching those them using [doctest](https://github.com/sol/doctest) and [`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md) libraries in the following sections.

1. Bootstrap a project using [`stack`](https://docs.haskellstack.org/en/stable/README/) first:

`stack new doctests-demo`

2. Go to the project's root directory and create a [Haskell module](https://www.haskell.org/tutorial/modules.html):

`cd doctests-demo`

`touch src/Sample.hs`

3. Add some dummy functions with doctests to newly created `Sample.hs` module:

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

### Library walkthroughs

#### `doctest`

[doctest](https://github.com/sol/doctest) is probably one of the most commonly used and actively maintained libraries.

Follow the steps below in order to launch the doctest examples in our `doctests-demo` project:

1. Install the library via `stack`:

`stack install doctest`

2. Go to the project's root directory and use the library's executable:

`cd doctests-demo`

`doctest src`

3. The command above should output something like the following:

`Examples: 2  Tried: 2  Errors: 0  Failures: 0`

#### Cons of using `doctest`

While being an awesome software, the library has some limitations and drawbacks:

1. It has a dependency on [GHC as a library](https://wiki.haskell.org/GHC/As_a_library) - it's very likely that something eventually breaks down when
you try to switch the project to a new compiler version.

2. When used via [`stack`](https://docs.haskellstack.org/en/stable/README/), one might need to create an additional test suite to bring the project
dependencies into the scope:

* Let's edit our doctest examples a little (don't forget to add `aeson` and `text` packages to the list of project dependencies):

<details>
<summary>Example</summary>

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

<hr>
</details>

* Now, if we try to run the doctests using `doctest src` command, we'll fail with the following message:

```
src/Sample.hs:9:1: error:
    Could not find module ‘Data.Aeson’
    Perhaps you meant Data.Version (from base-4.14.3.0)
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
    |
9 | import Data.Aeson
    | ^^^^^^^^^^^^^^^^^
```

* The workaround is to create an additional test suite:

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

* Next, add the test suite directory and `Main.hs` executable:

`mkdir doctests`

`touch doctests/Main.hs`

```haskell

-- doctests/Main.hs

import           Test.DocTest

-- This test suite exists only to add dependencies
main :: IO ()
main = doctest ["src"]
```

* Finally, run the doctests using the following command:

`stack test :doctests`

3. The library seems to be slow enough for large-scale projects.
One of the reasons causing the performance issue is that the library reloads the sources between each doctests' example group.
This is done to avoid those example groups depending on each other.
Using the `--fast` flag helps the situation a little, but this is not an option for the recent GHC versions.
You can learn more about this in the [library's readme](https://github.com/sol/doctest/blob/main/README.md).

### `cabal-docspec`

[`cabal-docspec`](https://github.com/phadej/cabal-extras/blob/master/cabal-docspec/MANUAL.md) library has certainly fewer contributors
involved and less overall community attention, but there're already a bunch of people using it in real projects.

#### Pros of using `cabal-docspec`

1. The library uses the compiled code - it makes it a lot faster than the alternatives.

2. The library doesn't depend on GHC as a library, so it's much more resilient to GHC version changes.

3. The library doesn't require recompiling the sources if it's only about writing doctests - this saves additional time for developers and
makes writing documentation pleasant.

All of the above makes it more than a viable alternative.

However, the library seems to be closely bound to [`cabal-install`](https://wiki.haskell.org/Cabal): it uses `cabal-install` generated metadata - the `plan.json` file.

Let's consider using `cabal-docspec` to run the doctests in our `doctests-demo` project:

1. First, setup `cabal-install` and the global compiler.
We suggest using [ghcup](https://www.haskell.org/ghcup/) to achieve this.

2. Build the project using `cabal`:

`cabal v2-build`

3. Download `cabal-docspec` binaries from [the releases page](https://github.com/phadej/cabal-extras/releases).
You can also consider building the library from source, following the instructions [here](https://github.com/phadej/cabal-extras).

```
curl -sL https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20211114/cabal-docspec-0.0.0.20211114.xz > cabal-docspec.xz
xz -d < cabal-docspec.xz > "$HOME"/.local/bin/cabal-docspec
rm -f cabal-docspec.xz
chmod a+x "$HOME"/.local/bin/cabal-docspec
```

4. Now, run the doctest examples using `cabal-docspec`:

`cabal-docspec`

5. The command above should fail with the following error:

```
expected: "{\"title\":\"One-Punch Man\",\"rating\":8.9}"
but got:
            ^
            <interactive>:10:1: error:
                Variable not in scope: encode :: Anime -> t
```

6. The above happens because the library handles things a little differently - it requires modules to be explicitly imported/exported.
Let's edit our doctests a little to make `cabal-docspec` work:

<details>
<summary>Workaround</summary>

```haskell

-- |
-- >>> import Data.Aeson
-- >>> encode favourite
-- "{\"title\":\"One-Punch Man\",\"rating\":8.9}"
--
favourite :: Anime
favourite = Anime "One-Punch Man" 8.9

```

<hr>
</details>

7. Now, the `cabal-docspec` command should succeed:

```
Total:         2; Tried:    2; Skipped:    0; Success:    2; Errors:    0; Failures    0
Examples:      2; Tried:    2; Skipped:    0; Success:    2; Errors:    0; Failures    0
```

Thanks for reading!
Subscribe to our newsletter to stay tuned - on the next part of this series, we will post about the configuration and internals of `cabal-docspec`!

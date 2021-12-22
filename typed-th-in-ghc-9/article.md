# Typed Template Haskell in GHC 9

Welcome to our third post about Template Haskell! Today we will take a look at the changes that were made in GHC 9 regarding Typed Template Haskell (TTH) and how to use the [`th-compat`](https://hackage.haskell.org/package/th-compat) library to make TTH code that will work with both GHC 8 and GHC 9.

In our [previous blog post](https://serokell.io/blog/typed-template-haskell-overview), we made an overview of Template Haskell in GHC 8. The article was later amended with the changes required so that the examples compile in GHC 9 in a non-backward compatible way. Make sure to read that post before you continue!

## Changed Typed Template Haskell specification

The `template-haskell` package was changed in version 2.17.0.0 and GHC version 9.0 according to the [Make Q (TExp a) into a newtype](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0195-code-texp.rst) proposal, in which the typed expression quasi-quoter (`[|| ... ||]`) now returns a different datatype.

Under the new specification, typed quotations now return `Quote m => Code m a` instead of `Q (TExp a)`. This is a breaking change for existing code, and codebases targeting GHC 9.0 either need to adapt their TTH code or use an alternative approach for compatibility.

The `Code` newtype is simply a wrapper around an `m (TExp a)`, defined similarly to this:

```hs
newtype Code m a = Code (m (TExp a))
```

The actual definition is a bit more complicated, as it contains some levity-polymorphism mechanisms. If you go to [its definition](https://hackage.haskell.org/package/template-haskell-2.18.0.0/docs/src/Language.Haskell.TH.Syntax.html#Code), you will actually see this:

```hs
type role Code representational nominal
newtype Code m (a :: TYPE (r :: RuntimeRep)) = Code
  { examineCode :: m (TExp a) -- ^ Underlying monadic value
  }
```

The differences between the simplified and real definitions are not important for the understanding of this post, and you may keep the simplified definition in mind while reading through this.

Thankfully, the untyped TH interface remains mostly unchanged, and the typed TH interface should only need to use `Code` instead of `TExp` to compile again with some helper functions.

<!-- TODO: Write about motivation as well? -->

## Example GHC 8 code

To make the examples here easier to follow, let us define a small typed Template Haskell module that we wish to work under GHC 8. It will serve as a basis so we can migrate it to work under GHC 9.

The code will be quite simple, its purpose is to parse an environment flag into a known data type or stop the compilation altogether if it fails.

We will use the [`template-haskell`](https://hackage.haskell.org/package/template-haskell) package, so don't forget to load it.

You will also need to activate the `TemplateHaskell` language extension.

```hs
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH.Syntax (Q, TExp)
import System.Environment (lookupEnv)

data LogLevel
  = Development  -- ^ Log errors and debug messages
  | Production  -- ^ Only log errors
  deriving (Show)

logLevelFromFlag :: Q (TExp LogLevel)
logLevelFromFlag = do
  flag <- liftIO $ lookupEnv "LOG_LEVEL"
  case flag of
    Nothing -> [|| Production ||]  -- We default to Production if the flag is unset
    Just level -> case level of
      "DEVELOPMENT" -> [|| Development ||]
      "PRODUCTION" -> [|| Production ||]
      other -> fail $ "Unrecognized LOG_LEVEL flag: " <> other
```

You can now query it from GHCi. To test it, `System.Environment` also exports the `setEnv :: String -> String -> IO ()` and `unsetEnv :: String -> IO ()` functions.

```hs
>>> :l TH
[1 of 1] Compiling TH               ( TH.hs, interpreted )
Ok, one module loaded.

>>> :set -XTemplateHaskell

>>> $$logLevelFromFlag
Production

>>> import System.Environment

>>> setEnv "LOG_LEVEL" "DEVELOPMENT"

>>> $$logLevelFromFlag
Development

>>> setEnv "LOG_LEVEL" "TH"

>>> $$logLevelFromFlag

<interactive>:311:1: error:
    • Unrecognized LOG_LEVEL flag: TH
    • In the Template Haskell splice $$logLevelFromFlag
      In the expression: $$logLevelFromFlag
      In an equation for ‘it’: it = $$logLevelFromFlag
```

If you try to compile this module with GHC 9, however, this example will fail to compile:

```hs
>>> :l TH

TH.hs:18:16: error:
    • Couldn't match type ‘Code m0’ with ‘Q’
      Expected: Q (TExp LogLevel)
        Actual: Code m0 LogLevel
    • In the Template Haskell quotation [|| Production ||]
      In the expression: [|| Production ||]
      In a case alternative: Nothing -> [|| Production ||]
   |
18 |     Nothing -> [|| Production ||]  -- We default to Production if the flag is unset
   |                ^^^^^^^^^^^^^^^^^^

TH.hs:20:24: error:
    • Couldn't match type ‘Code m1’ with ‘Q’
      Expected: Q (TExp LogLevel)
        Actual: Code m1 LogLevel
    • In the Template Haskell quotation [|| Development ||]
      In the expression: [|| Development ||]
      In a case alternative: "DEVELOPMENT" -> [|| Development ||]
   |
20 |       "DEVELOPMENT" -> [|| Development ||]
   |                        ^^^^^^^^^^^^^^^^^^^

TH.hs:21:23: error:
    • Couldn't match type ‘Code m2’ with ‘Q’
      Expected: Q (TExp LogLevel)
        Actual: Code m2 LogLevel
    • In the Template Haskell quotation [|| Production ||]
      In the expression: [|| Production ||]
      In a case alternative: "PRODUCTION" -> [|| Production ||]
   |
21 |       "PRODUCTION" -> [|| Production ||]
   |                       ^^^^^^^^^^^^^^^^^^
Failed, no modules loaded.
```

This is because the quasi-quoter now returns `Quote m => Code m LogLevel`.

In the next sections, we will see how to port this code to GHC 9 without and with backward compatibility with GHC 8.

## Porting old TTH code without backward compatibility

For this section, you need a GHC whose version is at least 9.

Make sure to import the appropriate definitions from `template-haskell`:

```hs
import Language.Haskell.TH.Syntax (Code, Q, examineCode, liftCode)
```

Since we now need to return a value of type `Code`, we change our type signature. And now we can fix the code simply by using two new functions, namely `examineCode` and `liftCode`:

```diff
-logLevelFromFlag :: Q (TExp LogLevel)
-logLevelFromFlag = do
+logLevelFromFlag :: Code Q LogLevel
+logLevelFromFlag = liftCode $ do  -- liftCode will turn 'Q' into 'Code'
  flag <- liftIO $ lookupEnv "LOG_LEVEL"
  case flag of
+    -- examineCode will turn 'Code' into 'Q'
-    Nothing -> [|| Production ||]  -- We default to Production if the flag is unset
+    Nothing -> examineCode [|| Production ||]  -- We default to Production if the flag is unset
    Just level -> case level of
_      "DEVELOPMENT" -> [|| Development ||]
_      "PRODUCTION" -> [|| Production ||]
+      "DEVELOPMENT" -> examineCode [|| Development ||]
+      "PRODUCTION" -> examineCode [|| Production ||]
      other -> fail $ "Unrecognized LOG_LEVEL flag: " <> other
```

The biggest problem here is the expectation that the quasi-quoter will return a `Code`. But at the same time, it has no `MonadIO`, `MonadFail` or `Quote` instances, preventing us from using `liftIO`, `fail`, and the quasi-quoter, which are provided by `Q`.

That is what the errors were telling us in the previous section. The solution is to use `examineCode` to turn a `Code m a` into a `m (TExp a)`, and then `liftCode` to do the opposite operation: turning a `m (TExp a)` into a `Code m a`.

```hs
liftCode    :: forall (r :: RuntimeRep) (a :: TYPE r) m. m (TExp a) -> Code m a
examineCode :: forall (r :: RuntimeRep) (a :: TYPE r) m. Code m a -> m (TExp a)
```

With it, our quasi-quoter result becomes in the expected format to run Template Haskell functions and then converted back into the type that the splice expects. The changes we made to the function are the general pattern you will use when exporting functions to be used by splices.

Additionally, you can also make the type signature a bit more generic:

```hs
logLevelFromFlag :: (MonadIO m, MonadFail m, Quote m) => Code m LogLevel
```

If you are only interested in using `Q`, however, you can instead use `CodeQ`, which is simply `Code Q`:

```hs
import Language.Haskell.TH.Lib (CodeQ)
```

And then use it like so:

```hs
logLevelFromFlag :: CodeQ LogLevel
```

And that's it! If you want the code to still work on GHC 8, however, make sure to read the next section.

## Porting old TTH code with backward compatibility

For this section, you may choose to use either GHC 8 or GHC 9.

We will use the [`th-compat`](https://hackage.haskell.org/package/th-compat) package, besides the familiar [`template-haskell`](https://hackage.haskell.org/package/template-haskell) package, so make sure you load them.

If you've read the previous section, you will see the pattern is pretty similar to the workflow with `Code`, albeit with `Splice` now.

Make sure to import the appropriate definitions first:

```hs
import Language.Haskell.TH.Syntax (Q)
import Language.Haskell.TH.Syntax.Compat (Splice, examineSplice, liftSplice)
```

And now just replace `Code` with `Splice`:

```diff
-logLevelFromFlag :: Code Q LogLevel
-logLevelFromFlag = liftCode $ do  -- liftSplice will turn 'Q' into 'Code'
+logLevelFromFlag :: Splice Q LogLevel
+logLevelFromFlag = liftSplice $ do  -- liftSplice will turn 'Q' into 'Splice'
  flag <- liftIO $ lookupEnv "LOG_LEVEL"
  case flag of
-    -- examineCode will turn 'Code' into 'Q'
-    Nothing -> examineCode [|| Production ||]  -- We default to Production if the flag is unset
+    -- examineSplice will turn 'Splice' into 'Q'
+    Nothing -> examineSplice [|| Production ||]  -- We default to Production if the flag is unset
    Just level -> case level of
-      "DEVELOPMENT" -> examineCode [|| Development ||]
-      "PRODUCTION" -> examineCode [|| Production ||]
+      "DEVELOPMENT" -> examineSplice [|| Development ||]
+      "PRODUCTION" -> examineSplice [|| Production ||]
      other -> fail $ "Unrecognized LOG_LEVEL flag: " <> other
```

And that's it! `Splice m a` is defined in `th-compat` as `m (TExp a)` in GHC 8 and `Code m a` in GHC 9, and this is why this code works. In addition, functions like `liftSplice` and `examineSplice` will either be defined `liftCode` and `examineCode` (in GHC 9) or `id` (in GHC 8).

If you wanted, you could drop your direct dependence on `template-haskell` here and simply use `th-compat`. You can use `SpliceQ` and avoid importing `Q` completely, similarly to `CodeQ`:

```hs
import Language.Haskell.TH.Syntax.Compat (SpliceQ, examineSplice, liftSplice)
```

And now use `SpliceQ` instead of `Splice`:

```hs
logLevelFromFlag :: SpliceQ LogLevel
```

This is because `SpliceQ` is defined simply as `Splice Q`.

It's also possible to use `Quote m => Splice m a`, however, `Quote` was also introduced in a recent `template-haskell` version, so chances are it won't work in GHC 8 and it will be pointless from the point of view of backward compatibility. It can still be useful in other situations, however, like in combination with monad transformers (e.g.: `Splice (State Int Q) a`).

## Parentheses in splices

Just one more thing before we conclude this article. You may see surprising behavior regarding the usage of parentheses in splices for both untyped and typed Template Haskell depending on whether you are using GHC 8 or GHC 9.

For example, if we had imported `logLevelFromFlag` qualified, then you'd write `$$(TH.logLevelFromFlag)` in GHC 8, otherwise you'd get a parser error:

```hs
>>> $$TH.logLevelFromFlag

<interactive>:200:1: error: parse error on input ‘$$’
```

But it works otherwise in GHC 9:

```hs
>>> $$TH.logLevelFromFlag
Production
```

## Conclusion

In this post, we've seen the changes introduced in GHC 9 regarding Typed Template Haskell. We learned about two different ways to migrate typed Template Haskell code from GHC 8 to GHC 9 and analyzed their differences.

Using `th-compat` has a big advantage in being compatible with two compilers, besides having an interface similar to the one in GHC 9. On the other hand, if supporting GHC 8 is not necessary, it becomes an extra dependency in your project, being potentially unfamiliar to many users. The choice of which strategy to use will ultimately depend on your specific needs, but we hope to have shed a light on their pros and cons.

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Medium](https://serokell.medium.com/).

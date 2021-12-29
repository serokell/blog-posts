# Typed Template Haskell in GHC 9

Welcome to our third post about Template Haskell! Today we will take a look at the changes that were made in GHC 9 regarding Typed Template Haskell (TTH) and how to use the [`th-compat`](https://hackage.haskell.org/package/th-compat) library to write TTH code that will work with both GHC 8 and GHC 9.

In our [previous blog post](https://serokell.io/blog/typed-template-haskell-overview), we gave an overview of Typed Template Haskell in GHC 8. The article was later amended with the changes required so that the examples compile in GHC 9 in a non-backward compatible way. Make sure to read that post before you continue!

## Changed Typed Template Haskell specification

The `template-haskell` package was changed in version 2.17.0.0 and GHC version 9.0 according to the [Make Q (TExp a) into a newtype](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0195-code-texp.rst) proposal, in which the typed expression quasi-quoter (`[|| ... ||]`) now returns a different datatype.

Under the new specification, typed quotations now return `Quote m => Code m a` instead of `Q (TExp a)`, and typed splices expect this new type as well. This is a breaking change for existing code, and codebases targeting GHC 9.0 either need to adapt their TTH code or use an alternative approach for compatibility.

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

## Example GHC 8 code

To make the examples here easier to follow, let us define a small typed Template Haskell module that we wish to work under GHC 8, which we will later port to GHC 9. The code will be quite simple, its purpose is to parse an environment flag into a known data type or stop the compilation altogether if it fails.

We will use the [`template-haskell`](https://hackage.haskell.org/package/template-haskell) package, so don't forget to load it. You will also need to activate the `TemplateHaskell` language extension.

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

If you try to compile this module with GHC 9, however, this example will fail to compile. We omit similar errors for brevity:

```text
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
```

Similarly, splices will also require that new code return `Code`, which is exemplified with a hole:

```text
>>> $$_

<interactive>:19:3: error:
    • Found hole: _ :: Code Q p0
      Where: ‘p0’ is an ambiguous type variable
    • In the Template Haskell splice $$_
      In the expression: $$_
      In an equation for ‘it’: it = $$_
```

In the following sections, we will see how to port this code to GHC 9 without and with backward compatibility with GHC 8.

## Porting old TTH code without backward compatibility

For this section, you need a GHC whose version is at least 9. Make sure to import the appropriate definitions from `template-haskell` as well:

```diff
-import Language.Haskell.TH.Syntax (Q, TExp)
+import Language.Haskell.TH.Syntax (Code, Q, examineCode, liftCode)
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
-      "DEVELOPMENT" -> [|| Development ||]
-      "PRODUCTION" -> [|| Production ||]
+      "DEVELOPMENT" -> examineCode [|| Development ||]
+      "PRODUCTION" -> examineCode [|| Production ||]
      other -> fail $ "Unrecognized LOG_LEVEL flag: " <> other
```

The errors in the previous section happened because the type returned by the quotations is no longer a `Q`, but `Code` instead. The solution is to use `examineCode` to turn a `Code Q a` into a `Q (TExp a)`, and then `liftCode` to do the opposite operation: turning a `Q (TExp a)` into a `Code Q a`.

```hs
liftCode    :: forall (r :: RuntimeRep) (a :: TYPE r) m. m (TExp a) -> Code m a
examineCode :: forall (r :: RuntimeRep) (a :: TYPE r) m. Code m a -> m (TExp a)
```

And that's it! If you want the code to still work on GHC 8, however, make sure to read the next section.

## Porting old TTH code with backward compatibility

For this section, you may choose to use either GHC 8 or GHC 9. We will use the [`th-compat`](https://hackage.haskell.org/package/th-compat) package, besides the familiar [`template-haskell`](https://hackage.haskell.org/package/template-haskell) package, so make sure you load them both.

`th-compat` is our recommended library for backward compatibility with TTH. In this section, we will show how to change the usage from `Code` to the backward-compatible `Splice` which works on GHC 9 as well as previous versions.

The pattern is pretty similar to the workflow with `Code`, albeit with `Splice` now.

Make sure to import the appropriate definitions first:

```diff
-import Language.Haskell.TH.Syntax (Code, Q, examineCode, liftCode)
+import Language.Haskell.TH.Syntax (Q)
+import Language.Haskell.TH.Syntax.Compat (Splice, examineSplice, liftSplice)
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

And that's it! `Splice m a` is defined in `th-compat` as `Code m a` in GHC 9 and as `m (TExp a)` in prior versions, and this is why this code works. In addition, functions like `liftSplice` and `examineSplice` will either be defined as `liftCode` and `examineCode` (in GHC 9) or `id` (in GHC 8 and below).

## Parentheses in splices

Just one more thing before we conclude this article. You may see surprising behavior regarding the usage of parentheses in splices for both untyped and typed Template Haskell. The parser was tweaked in GHC 9 so parentheses are not necessary in some situations compared to prior versions.

For example, if we had imported `logLevelFromFlag` qualified, then you'd write `$$(TH.logLevelFromFlag)` in GHC 8, otherwise you'd get a parser error:

```hs
>>> $$TH.logLevelFromFlag

<interactive>:200:1: error: parse error on input ‘$$’
```

But it works in GHC 9:

```hs
>>> $$TH.logLevelFromFlag
Production
```

## Conclusion

In this post, we've seen the changes introduced in GHC 9 regarding Typed Template Haskell. We've learned about two different ways to migrate typed Template Haskell code from GHC 8 to GHC 9 and analyzed their differences.

Using `th-compat` has a big advantage in being compatible with various GHC versions, besides having an interface similar to the one in GHC 9. On the other hand, if supporting GHC 8 is not necessary, it becomes an extra dependency in your project that's potentially unfamiliar to many users. The choice of which strategy to use will ultimately depend on your specific needs, but we hope to have shed light on their pros and cons.

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Dev](https://dev.to/serokell/).

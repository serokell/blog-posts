# Handling of uncaught exceptions in Haskell

When your Haskell application's thread throws an exception that does not get caught, the Haskell runtime system will handle it and print it based on the `Show` instance.
This is the default behavior that can be customized using the [`setUncaughtExceptionHandler`](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Conc-Sync.html#v:setUncaughtExceptionHandler) function.

Personally, I was quite surprised when I noticed that the `Show` type class is used for rendering.
At the early days of my Haskell development experience, I noticed that the `Exception` type class had the `displayException` method and assumed that it's used for printing of uncaught exceptions.
A few years later, I realized that my assumption was wrong and was quite surprised.
The default uncaught exception handler uses `showsPrec` from the `Show` type class, not `displayException`.
When I shared this information with my colleagues, some of them were surprised as well.
I was inclined to think that `displayException` is a better default, and it turned out that I was not the only one thinking this way.
Since people were supporting this idea, I decided to do two things:
1. Raise this topic in the GHC issue tracker.
2. Create a small library with a function that would carefully modify uncaught exception handling to use `displayException`.
We already had such a helper in our code base and wanted to extract it into a library.
This function is not entirely trivial because, for example, `ExitCode` exception should be handled specially (the program should exit with the appropriate exit code in this case).

I started with researching this topic and discovered that it already had been discussed back in 2014.

## History

The relevant GHC issue is [#9822](https://gitlab.haskell.org/ghc/ghc/-/issues/9822).
It refers to the [e-mail thread](https://mail.haskell.org/pipermail/libraries/2014-November/024176.html) where two things were proposed:
1. The addition of `displayException` to `Exception`.
2. The modification of the default uncaught exception handler to use `displayException`.

The first thing was accepted and implemented, and the `displayException` is a part of `Exception` for many years already.
The second thing, however, appeared to be debatable and there was no consensus with regards to it.
You can read the whole thread for details since it's not so big.
Two essential points from it are:
1. There is a good reason to use `show` for uncaught exceptions.
Uncaught exceptions are usually encountered by programmers, so it's better to format them in "programmer-friendly" way, i. e. using `show`.
So that should be the default.
However, sometimes one may prefer `displayException`, so it should be easy to use it instead.
2. Even though there is [`setUncaughtExceptionHandler`](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Conc-Sync.html#v:setUncaughtExceptionHandler), modifying the uncaught exception handler to use `displayException` is not easy because:
  * stdout has to be flushed, ignoring any exceptions while doing so;
  * output has to go to stderr, not stdout;
  * `ExitCode` exception should be handled correctly, and the program should exit with correct exit code.

After that, there was a discussion about adding `useDisplayExceptionHandler` or `displayExceptionHandler` that would do "the right thing", but this discussion quickly moved into another direction and no such function was added.
In 2020, I couldn't find a function that does "the right thing" on Hackage.

When I first implemented this helper (before reading this thread), I didn't take `ExitCode` into account and had a subtle minor bug.
My program was exiting with non-zero code when launched with `--help` because `optparse-applicative` library throws (probably indirectly) `ExitSuccess` in this case, but I didn't consider that.

## Why `displayException`?

The intended difference between `displayException` and `show` is that the former should return a "human-friendly" string, while the latter does not say anything about "human-friendliness" and is usually auto-generated to produce "programmer-friendly" strings that reflect the internal structure of the Haskell type.
A good analogy from the aforementioned e-mail thread is `str()` and `repr()` functions in Python.

Based on my experience, I admit that `displayException` is usually implemented as `show`, which is the default implementation.
I think there are 3 possible reasons for that:
1. `Show` is auto-generated and the string produced by it sufficiently human-friendly and custom formatting is not needed.
I believe it's a rare case because auto-generated `show` usually looks quite verbose.
2. A programmer just didn't bother implementing `displayException`.
Maybe they forgot to do it (the compiler doesn't check it), maybe they were lazy, maybe they didn't even know about this method.
3. There is a custom `Show` instance with human-readable pretty formatting.
I think it's a bad idea because then there is no way to render an exception in "programmer-friendly" way.
"Programmer-friendly" way usually contains Haskell identifiers, so that you can quickly find documentation for the relevant data type.
Also, you should be able to see all values that constitute the exception.
"Human-friendly" way may return a string where some fields are omitted and figuring out which type of exception was thrown can be complicated.

So currently, in many cases it does not really matter which function is called to print an uncaught exception because `displayException` is often implemented as `show`.
However, I think that in the ideal world, most of the types with `Exception` instance should provide a custom definition for `displayException`, and the topic brought up in this article will matter.

It's indeed debatable which function is more appropriate. I think there is no clear winner, so it should be easy to use either of them for uncaught exceptions.
One thought that I have is that `displayException` is more appropriate for console apps because uncaught exceptions are visible to end users, while `show` is more appropriate for UI apps because errors in UI apps are hidden from end users.
Anyway, it's just one thought, and I don't intend to recommend using `displayException` for uncaught exceptions, I just want to make it easy to do so if someone wants that.

## uncaught-exception

Since there was a discussion about providing a function that would change uncaught exception handling to use `displayException` but no actions were taken, I went ahead and implemented a tiny library for this use case.
The library is called [uncaught-exception](https://hackage.haskell.org/package/uncaught-exception).
You can view its source code on [GitHub](https://github.com/serokell/uncaught-exception).

The default exception handler is implemented in GHC [as follows](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Conc.Sync.html#uncaughtExceptionHandler):
```
defaultHandler :: SomeException -> IO ()
defaultHandler se@(SomeException ex) = do
   (hFlush stdout) `catchAny` (\ _ -> return ())
   let msg = case cast ex of
         Just Deadlock -> "no threads to run:  infinite loop or deadlock?"
         _                  -> showsPrec 0 se ""
   withCString "%s" $ \cfmt ->
    withCString msg $ \cmsg ->
      errorBelch cfmt cmsg

-- don't use errorBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h errorBelch2"
   errorBelch :: CString -> CString -> IO ()
```

I could have implemented a similar handler myself, but decided not to do it because I don't entirely understand this definition.
Specifically:
1. How is `ExitCode` handled and how does a Haskell program determine which code it should exit with?
Is it handled by `errorBelch`? Probably no because `errorBelch` only takes two strings.
So perhaps it's handled outside of this helper.
2. What does `errorBelch` do, and why is it implemented in a foreign language (most likely C/C++)?
Can't it be implemented in Haskell?
Should I use FFI in my handler as well?

I guess I could find out answers to these questions, but I picked a simpler way, actually two ways.
If this default handler is ever updated in GHC, I will not have to do anything in my library.
I define the following wrapper:

```
-- | Helper data type used by 'displayUncaughtException'.
-- It causes @show@ to call @displayException@.
-- When an exception of this type is caught, it will be @show@n and
-- that will call @displayException@ of the wrapped exception.
newtype DisplayExceptionInShow = DisplayExceptionInShow SomeException

instance Show DisplayExceptionInShow where
  show (DisplayExceptionInShow se) = displayException se

instance Exception DisplayExceptionInShow
```

along with the `wrapException :: SomeException -> SomeException` function that wraps exceptions into this wrapper.
And define two functions:
```
displayUncaughtException :: IO a -> IO a
displayUncaughtException = handle (throwIO . wrapException)

withDisplayExceptionHandler :: IO a -> IO a
withDisplayExceptionHandler action = do
  handler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler (handler . wrapException)
  action <* setUncaughtExceptionHandler handler
```

They serve the same purpose, but I am not sure which one is better, hence I provide both.
If you have a good argument in favor of one of these functions, please let me know.

In the first version, we don't touch the default handler at all.
In the second version, we only update it to wrap its argument into `DisplayExceptionInShow` and then apply as is.
Also, there is `setDisplayExceptionHandler`, which updates the handler the same way as `withDisplayExceptionHandler` but doesn't restore it in the end.
In all cases, all concerns should be handled.

All but one: if we wrap `ExitCode` into `DisplayExceptionInShow`, we will break functions like `exitSuccess` because they work by throwing an `ExitCode` exception and we change its type.
So we implement `wrapException` carefully to ignore `ExitCode`:
```
wrapException :: SomeException -> SomeException
wrapException e
  | isSyncException e
  , Nothing <- fromException @Deadlock e
  , Nothing <- fromException @ExitCode e =
      toException $ DisplayExceptionInShow e
  | otherwise = e
  where
    isSyncException :: SomeException -> Bool
    isSyncException = isNothing . fromException @SomeAsyncException
```

Notice that we also ignore exceptions that can be cast to `SomeAsyncException`.
The reason is twofold:
1. There are not many such exceptions, and their `show` representation should be more or less human-friendly.
2. I am afraid that by wrapping them into `DisplayExceptionInShow`, we may break something the same way as we break when we wrap `ExitCode`.

We also ignore `Deadlock` because it's specifically handled by `defaultHandler`.

Notice that there is an essential difference in case an uncaught exception occurs in a thread other than the one where one of these functions was used.
`displayUncaughtException` only affects the thread where it's called, all other threads are unaffected (and use `Show` for printing by default).
`withDisplayExceptionHandler` updates the global variable holding the uncaught exception handler and thus affects all threads in the application.
The same is true for `setDisplayExceptionHandler`.
Since the uncaught exception handler is stored in a global variable, there can be race conditions when multiple threads are involved.
For example, if an action inside `withDisplayExceptionHandler` forks a thread and this thread ends with an exception, error printing may be unpredictable (depends on what action finishes first).
A good practice is to use functions from the [`async`](https://hackage.haskell.org/package/async) package, such as `concurrently` or `withAsync`:
* `withAsync` ensures that the child thread always stops before the parent thread;
* `concurrently` additionally ensures that exceptions thrown by children threads are propagated to their parent.

Further discussions about the `async` packages are out of the scope of this article.

## Conclusion

Several years ago, a new method was added to the `Exception` type class: `displayException`.
It was supposed to be used for printing uncaught exceptions, but this idea appeared to be controversial and didn't get enough support.
As an alternative, it was proposed to add helpers that one would use to change the default uncaught exception handler to use `displayException`.
However, to the best of my knowledge, no such helpers exist nowadays, neither in `base` nor in a dedicated library.

In this article, I present a new library with such helpers.
I am not 100% sure my functions handle all cases correctly, maybe there are other exceptions that need special treatment.
If you have any suggestions regarding the implementation of this library, please don't hesitate to open an issue/PR in the repo.

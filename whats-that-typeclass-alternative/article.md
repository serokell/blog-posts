You may come across a programming task where you want to implement the following pattern: try _this_ and, if it doesn't work out, try _the other thing_. 

In other words, you want a computation composed of two (or more) alternatives which should be tried
one after the other until one of them produces a useful result.

As an example, let's build a function that takes a `String` as input and returns `Just String` if that input is either `"bob"` or `"alice"`.

First, we'll define two functions:

```haskell
isAlice :: String -> Maybe String
isAlice a = if a == "alice" then Just a else Nothing

isBob :: String -> Maybe String
isBob a = if a == "bob" then Just a else Nothing
```

We can then compose them like this:

```haskell
isAliceOrBob :: String -> Maybe String
isAliceOrBob a =
  case isAlice a of
    Just _ -> Just a
    Nothing -> case isBob a of
                 Just _ -> Just a
                 Nothing -> Nothing
```

```none
λ> isAliceOrBob "alice"
Just "alice"
λ> isAliceOrBob "bob"
Just "bob"
λ> isAliceOrBob "charly"
Nothing
```

This works fine for two functions, but becomes ugly when we add more alternatives. 

```haskell
isCharly :: String -> Maybe String
isCharly a = if a == "charly" then Just a else Nothing

isAliceOrBoborCharly :: String -> Maybe String
isAliceOrBoborCharly a =
  case isAlice a of
    Just _ -> Just a
    Nothing -> case isBob a of
                 Just _ -> Just a
                 Nothing -> case isCharly a of 
                              Just _ -> Just a 
                              Nothing -> Nothing
```

It would be better if we could use an operator (such as `<|>`) for this pattern:

```haskell
isAliceOrBobOrCharly :: String -> Maybe String
isAliceOrBobOrCharly a = isAlice a <|> isBob a <|> isCharly a
```

Actually, the code above is valid Haskell. All you need to do to 
make use of the `<|>` operator is to import it from [`Control.Applicative`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Applicative.html).

It's the main method of the [`Alternative`](https://hackage.haskell.org/package/monadplus-1.4.2/docs/Control-Applicative-Alternative.html) typeclass, which we'll cover in this article.

## The Alternative typeclass

Let's look at what GHCi can tell us about the `Alternative` typeclass:

```haskell
type Alternative :: (* -> *) -> Constraint
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
  	-- Defined in ‘GHC.Base’
instance Alternative ZipList -- Defined in ‘Control.Applicative’
instance Alternative [] -- Defined in ‘GHC.Base’
instance Alternative Maybe -- Defined in ‘GHC.Base’
instance Alternative IO -- Defined in ‘GHC.Base’
```

This gives us some useful facts:

1. A type implementing `Alternative` must be of [kind](https://serokell.io/blog/kinds-and-hkts-in-haskell) `* -> *`.
2. It must also implement `Applicative` (and, with that, also `Functor`).
3. To implement it, you must implement `<|>` and `empty`.
4. In addition to those methods, it gives you `some` and `many`, which we'll discuss further below.

### Alternative for Maybe

`Alternative` is already defined for the `Maybe` type that we used in the example at the start.

Let's take a look at the implementation:

```haskell
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l
```

If you take a look at the definitions, you can see that `empty :: f a` is
the identity of `(<|>) :: f a -> f a -> f a`, very much like `mempty :: a` is
the identity of `mappend :: a -> a -> a` in case of `Monoid`.

In fact, `Alternative` is said to be _a monoid on applicative functors_.

The truth table of `Alternative Maybe` looks like this:

|             | **Nothing** | **Just r** |
|-------------|-------------|------------|
| **Nothing** | Nothing     | Just r     |
| **Just l**  | Just l      | Just l     |


### Alternative for IO

Another interesting `Alternative` instance is `IO`.

Here, the first alternative from left to right that succeeds wins:

```haskell
test :: IO String
test = fail "foo" <|> pure "bar" <|> fail "baz"
```

When executed, the first alternative (`fail "foo"`) fails. The second one
(`pure "bar"`) succeeds, which is why the third isn't executed at all.

```none
λ> test
"bar"
```

## A simple parser using Alternative

A very common use case of the `Alternative` typeclass is in parsers.

Let's assume we want to implement a simple parser that reads a
stream of type 'i' and possibly returns a value of type `a` if the input matches.

```haskell
newtype Parser i a
  Parser { runParser :: [i] -> (Maybe a, [i]) }
```

This `Parser` type wraps a function that takes an input and gives back
`Just a` if it matches and `Nothing` if it doesn't, together with the
remaining unused input.

```haskell
evalParser :: (Show i) => Parser i a -> [i] -> Either String a
evalParser p is = case runParser p is of
                    (Nothing, is) -> Left $ "parser failed on " ++ show is
                    (Just a, _)   -> Right a
```
`evalParser` runs such a parser and returns a more convenient `Either` with an error string or the result.

A concrete implementation of the parser could, for example,
consume a numeric input and succeed if it is smaller than a given number
or fail if it is equal or greater than that number:

```haskell
parseLT :: Ord a => a -> Parser a a
parseLT a = Parser f
    where f input@(i : is)
              | i < a     = (Just i, is)     -- parser succeeds
              | otherwise = (Nothing, input) -- parser fails
          f input         = (Nothing, input) -- empty input - parser also fails
```

Running it shows that it works as expected:

```
λ> evalParser (parseLT 10) [2,3,10,20]
Right 2
λ> evalParser (parseLT 10) [10,2,3,20]
Left "parser failed on [10,2,3,20]"
λ> evalParser (parseLT 10) []
Left "parser failed on []"
```

### Implementing an Alternative instance for our parser

First, we need a [`Functor`](https://serokell.io/blog/whats-that-typeclass-functor)
instance for our `Parser i a` type, which is pretty
straightforward and reuses the `Functor` instance for our inner `Maybe a`.

```haskell
{-# LANGUAGE InstanceSigs  #-}

instance Functor (Parser i) where
    fmap :: (a -> b) -> Parser i a -> Parser i b
    fmap f (Parser p) =
        Parser $ \is ->
            let (aM, rest) = p is -- run the parser
            in (fmap f aM, rest)  -- reuse the functor instance on the 'Maybe a`
```

**Note:** Above, I've explicitly written the `fmap` function signature in the instance
declaration. It isn't required but it makes it easier to understand what
is going on. To do that, you need to enable the [`InstanceSigs`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#instance-signatures-type-signatures-in-instance-declarations)
language extension.

Before we can implement `Alternative`, we also need to implement
`Applicative`:

```haskell
instance Applicative (Parser i) where
    pure :: a -> Parser i a
    pure a = Parser (\is -> (Just a, is))
    -- with TupleSections could be written as : Parser (Just a,)

    (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
    (Parser f) <*> (Parser p) = Parser $ \is ->
      let (fM, rest) = f is
      in case fM of
           Nothing -> (Nothing, rest)
           Just f' -> case p rest of
                        (Just o, rest')  -> (pure $ f' o, rest')
                        (Nothing, rest') -> (Nothing, rest')
```

I won't go into the details of `Applicative` here, as there are [excellent 
articles](https://serokell.io/blog/whats-that-typeclass-applicative) on the idea
and use of applicative functors. But with that out of the way, we're finally able
to define the `Alternative` typeclass for our parser.

```haskell
instance Alternative (Parser i) where
    empty = Parser (\is -> (Nothing, is))
    -- with TupleSections could be written as: Parser (Nothing,)

    (<|>) :: Parser i a -> Parser i a -> Parser i a
    (Parser pa) <|> (Parser pb) = Parser $ \is ->
      let (aMa, restA) = pa is
          (aMb, restB) = pb is
      in case (aMa, aMb) of
           (Just a, _)       -> (Just a, restA)
           (Nothing, Just b) -> (Just b, restB)
           _                 -> (Nothing, is)
```

But why do we want an `Alternative` instance for the parser?
First of all, because of the possibility to compose multiple `Parser i a` with
the `<|>` operator.

Another reason is the two methods of `Alternative` that we haven't
discussed so far, which we get for free:

* `some :: f a -> f [a]` which means: _one or more_
* `many :: f a -> f [a]` which means: _zero of more_

In our case, `some (parseLT 10)` will create a new parser of type
`parseLT :: Int -> Parser Int [Int]` that expects at least one integer
that is smaller than ten and will consume all the following numbers which are
smaller than ten as well.

`many (parseLT 10)` is similar, but it doesn't fail if the first element in the input doesn't match.
In that case, it will just return the empty list `[]`.

Let's try out these methods:

```none
λ> evalParser (some $ parseLT 10) [2,3,10,20]
Right [2,3]

λ> evalParser (many $ parseLT 10) [2,3,10,20]
Right [2,3]

λ> evalParser (some $ parseLT 10) [10,2,3,20]
Left "parser failed on [10,2,3,20]"

λ> evalParser (many $ parseLT 10) [10,2,3,20]
Right []
```

`some` and `many` are very common and useful patterns in parser combinators.

If you want to dive deeper into the topic of parser combinators in Haskell, take a look
[at this post](https://serokell.io/blog/parser-combinators-in-haskell).

## MonadPlus

There is a typeclass called [`MonadPlus m`](https://wiki.haskell.org/MonadPlus)
which has the same semantics as `Alternative` but for types that implement `Monad`.
So it can be seen as a "monoid on monads". It comes with a default implementation
based on `Alternative`, so you don't have to implement any of the methods.

```haskell
-- The MonadPlus class definition

-- | Monads that also support choice and failure.
class (Alternative m, Monad m) => MonadPlus m where
   -- | The identity of 'mplus'.  It should also satisfy the equations
   --
   -- > mzero >>= f  =  mzero
   -- > v >> mzero   =  mzero
   --
   -- The default definition is
   --
   -- @
   -- mzero = 'empty'
   -- @
   mzero :: m a
   mzero = empty

   -- | An associative operation. The default definition is
   --
   -- @
   -- mplus = ('<|>')
   -- @
   mplus :: m a -> m a -> m a
   mplus = (<|>)
```

Although you could use the `Alternative` machinery everywhere where you could use `MonadPlus`
(as every `Monad` is also an `Applicative`), there are useful functions that only work in the
context of a `MonadPlus`. For example, [`mfilter`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad.html#v:mfilter),
which filters a `MonadPlus` with a predicate.

```
λ> mfilter odd (Just 1)
Just 1
λ> mfilter odd (Just 2)
Nothing
λ> mfilter odd (Nothing)
Nothing
```

## Summary

* `Alternative` is a useful instance to implement for your applicative functor
  if it has a semantic of _try this or, alternatively, that_.
* `Alternative` can be seen as a monoid on applicative functors. In the
  `Data.Monoid` module there even exists a
  [wrapper `Alt`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Monoid.html)
  that further shows this idea.
* If you implement a parser (combinator), you almost always want to implement
  an `Alternative` instance for it.

If you would like to read more Haskell articles about topics like this, be sure to follow us on [Twitter](https://twitter.com/serokell) or subscribe to our newsletter via the form below.

## Exercises

1. Implement the `isAliceOrBobOrCharly` example from the beginning in terms of
   our new `Parser i a` type.

   So you come up with a parser:

   ```haskell
   parseAliceBobCarly :: Parser Char String
   parseAliceBobCarly = ...
   ```

   You're allowed to utilize functions from `Data.List`.

   <details>
   <summary>Solution</summary>

   ```haskell
   parseString :: Eq i => [i] -> Parser i [i]
   parseString a = Parser f
     where f input = case stripPrefix a input of
                       Nothing   -> (Nothing, input)
                       Just rest -> (Just a, rest)


   parseAliceBobCarley :: Parser Char String
   parseAliceBobCarley =
       parseString "alice" <|>
       parseString "bob" <|>
       parseString "charly"
   ```

   As mentioned above, there is a very close relationship between `Alternative` and
   `Monoid` typeclasses, which provides the semantics of _concatenation_ of
   elements of a list using  `Monoid`s `mappend`
   named [`mconcat`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:mconcat).
   The `Data.Foldable` module provides a useful function with the same semantics
   on `Alternative` named
   [`asum :: (Foldable t, Alternative f) => t (f a) -> f a`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Applicative.html#v:asum). With that, we could write the function above also as:

   ```haskell
   parseAliceBobCarley' :: Parser Char String
   parseAliceBobCarley' = asum [ parseString "alice", parseString "bob", parseString "charly"]
   ```

   <hr />
   </details>

2. To be able to give the user better information about what went wrong
   in case of parser failure (parser didn't match? parser reached the end of input?),
   replace the inner `Maybe a` with a `Either err a` and fix class instances:

   ```haskell
   newtype Parser2 err i a =
     Parser { runParser2 :: [i] -> (Either err a, [i]) }
   ```


   <details>
   <summary>Solution</summary>

   ```haskell
   newtype Parser2 err i a =
     Parser2 { runParser2 :: [i] -> (Either err a, [i]) }

   instance Functor (Parser2 err i) where
       fmap f (Parser2 p) =
           Parser2 $ \is ->
               let (aE, rest) = p is
               in (f <$> aE, rest)

   instance Applicative (Parser2 err i) where
       pure a = Parser2 (Right a,) -- TupleSections: Parser (\is -> (Just a, is))

       (Parser2 f) <*> (Parser2 p) = Parser2 $ \is ->
         let (fE, rest) = f is
         in case fE of
              Left err -> (Left err, rest)
              Right f' -> case p rest of
                           (Right o, rest')  -> (pure $ f' o, rest')
                           (Left err, rest') -> (Left err, rest')

   data Parser2Error
     = EmptyParser
     | NoInput
     | ParserFailure
     deriving (Eq, Show)

   instance Alternative (Parser2 Parser2Error i) where
       empty = Parser2 (Left EmptyParser, )

       (Parser2 pa) <|> (Parser2 pb) = Parser2 $ \is ->
         let (aEa, restA) = pa is
             (aEb, restB) = pb is
         in case (aEa, aEb) of
              (Right a, _) -> (Right a, restA)
              (_, Right b) -> (Right b, restB)
              _            -> (Left ParserFailure, is)


   evalParser2 :: (Show i) => Parser2 Parser2Error i a -> [i] -> Either String a
   evalParser2 p is = case runParser2 p is of
                       (Left err, is) -> Left $ "parser failed with " <> show err <> " on " ++ show is
                       (Right a, _)   -> Right a

   ```
   <hr />
   </details>

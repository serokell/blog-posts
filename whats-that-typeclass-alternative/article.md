You may come across a programming task where you want to implement something like:

Try _this_ and, if it doesn't work out, try _that_ - or one could say a computation composed of two (or more) alternatives which should be tried
one after the other until one of them produces a useful result.

As an example, lets build a function which takes a `String` as input and returns `Just String` if that input is either `"bob"` or `"alice"`.

Although there might be a lot of other ways to solve this task, we'll do it by defining two functions:

```haskell
isAlice :: String -> Maybe String
isAlice a = if a == "alice" then Just a else Nothing

isBob :: String -> Maybe String
isBob a = if a == "bob" then Just a else Nothing
```

We can then compose these two functions like this:
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

This works well, but it will become ugly when we add more alternatives to the computation  f.e. a `isCharly :: String -> Maybe String`.

It would be nice if we could have an operator to write it in a nice way like this:

```haskell
isCharly :: String -> Maybe String
isCharly a = if a == "charly" then Just a else Nothing

isAliceOrBobOrCharly :: String -> Maybe String
isAliceOrBobOrCharly a = isAlice a <|> isBob a <|> isCharly a
-- or
isAliceOrBobOrCharly a = isAliceOrBob a <|> isCharly a
```


## The Alternative typeclass

Actually, the code above is valid Haskell. All you need to do to make use of the `<|>` operator is to import it from `Control.Aplicative`:

```none
λ> :info (<|>)
type Alternative :: (* -> *) -> Constraint
class Applicative f => Alternative f where
  ...
  (<|>) :: f a -> f a -> f a
  ...
  	-- Defined in ‘GHC.Base’
infixl 3 <|>
```

It's a method of the `Alternative` typeclass. Let's look at what information GHCI can provide us about this typeclass:

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

This already gives us a lot of useful facts:

1. A type implementing `Alternative` must be of [kind](https://serokell.io/blog/kinds-and-hkts-in-haskell) `* -> *`.
2. It must also implement `Applicative` (and, with that, also `Functor`).
3. To implement it, you must implement `<|>` and `empty`.
4. In addition to those methods, it gives you `some` and `many`, which we'll discuss further below.
5. It's already defined for the `Maybe` type that we used in the example above.

Lets take a look on the implementation of `Alternative Maybe`:

```haskell
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l
```

If you take a look at the definitions you can see that `empty :: f a` is
the identity of `<|>` (very much like `mempty :: a` is the identity of
`mappend :: a -> a -> a` in case of `Monoid`.) `Alternative` is also
said to be _a monoid on applicative functors_.

The truth table of `Alternative Maybe` looks like this:

|             | **Nothing** | **Just r** |
|-------------|-------------|------------|
| **Nothing** | Nothing     | Just r     |
| **Just l**  | Just l      | Just l     |


Another interesting `Alternative` instance is `IO`.
Here, the first alternative from left to right that succeeds wins, f.e.

```haskell
test :: IO String
test = fail "foo" <|> pure "bar" <|> fail "baz"
```

When executed the first computation `fail "foo"` obviously fails, so the second one `pure "bar"` succeeds, which is why the third `fail "baz"` isn't executed at all.

```none
λ> test
"bar"
```

## A simple parser using Alternative

A very common use case of the `Alternative` typeclass is in parsers.

Lets assume we want to implement a simple parser which reads a stream of 'i' and possibly returns a value of type `a` if the input matches.

```haskell
newtype Parser i a
  Parser { runParser :: [i] -> (Maybe a, [i]) }

evalParser :: (Show i) => Parser i a -> [i] -> Either String a
evalParser p is = case runParser p is of
                    (Nothing, is) -> Left $ "parser failed on " ++ show is
                    (Just a, _)   -> Right a
```

This `Parser` type wraps a function which takes an input and gives back `Just a` in case it matches and `Nothing` if it doesn't, together with the remaining unused input.

`evalParser` runs such a parser and returns a more convenient `Either` with
an error string or the result.


An implementation of the parser could be one which consumes a numeric input
and succeeds if it smaller then a given number of fails if it is equal
or greater that number:


```haskell
parseLT :: Ord a => a -> Parser a a
parseLT a = Parser f
    where f input@(i : is)
              | i < a     = (Just i, is)     -- parser succeeds
              | otherwise = (Nothing, input) -- parser fails
          f input         = (Nothing, input) -- empty input - parser also fails
```

Running it shows the expected results:

```
λ> evalParser (parseLT 10) [2,3,10,20]
Right 2
λ> evalParser (parseLT 10) [10,2,3,20]
Left "parser failed on [10,2,3,20]"
λ> evalParser (parseLT 10) []
Left "parser failed on []"
```

### Implementing an Alternative instance for our parser


First we need a `Functor` instance for our `Parser i a` type, which is pretty
straightforward and reuses the `Functor` instance for our inner `Maybe a`.

For more details on the `Functor` type class check out
[this article](https://serokell.io/blog/whats-that-typeclass-functor).

```haskell
{-# LANGUAGE InstanceSigs  #-}

instance Functor (Parser i) where
    fmap :: (a -> b) -> Parser i a -> Parser i b
    fmap f (Parser p) =
        Parser $ \is ->
            let (aM, rest) = p is -- run the parser
            in (fmap f aM, rest)  -- reuse the functor instance on the 'Maybe a`
```

Note: Above I've explicitly written the `fmap` function signature in the instance
deceleration. It isn't required but makes it more easy to understand what
is going on. To be able to do that, you need to enable the `InstanceSigs` language extension.

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

I won't go into the details of `Applicative` here, as there are [excellent other
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

But why should we actually want an `Alternative` instance for the parser?
First of all, because of the possibility to compose multiple `Parser i a` with
the `<|>` operator.

Another reason is two methods of `Alternative` that we haven't
discussed so far, which we get for free:

* `some :: f a -> f [a]` which means: _one or more_
* `many :: f a -> f [a]` which means: _zero of more_

In our case, `some (parseLT 10)` will create a new parser of type
`parseLT :: Int -> Parser Int [Int]` that expects at least one integer
that is smaller 10 and then will consume all following numbers which are
smaller 10 as well.

In case of `many (parseLT 10)`, the type signature looks the same as in the
`some` case, but it does not fail if the first element in the input isn't a
number smaller 10. In that case, it will just return the empty list `[]`.

So lets try out these methods:

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

`some` and `many` are a very common and useful pattern in parser combinators.

If you want to dive deeper into the topic of parsers in Haskell, take a look
[at this post](https://serokell.io/blog/parser-combinators-in-haskell).

## Conclusion and outlook

* `Alternative` is a useful instance to implement for your applicative functor
  if it has a semantic of _try this or alternatively that_.
* `Alternative` can be seen as a monoid on applicative functors. In the
  `Data.Monoid` module there even exists a
  [wrapper `Alt`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Monoid.html)
  which further shows this idea.
* If you implement a parser (combinator), you almost always want to implement
  an `Alternative` instance for it.


Another very closely related typeclass is [`MonadPlus m`](https://wiki.haskell.org/MonadPlus)
which has the same semantics as `Alternative` but for types which also implement `Monad`.
So it can be seen as a "monoid on monads". It comes with a default implementation
based on `Alternative`, so you don't have to actually implement any methods:

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

Altough you could use the `Alternative` machinery everywhere where you could use `MonadPlus`
(as every `Monad` is also an `Applicative`) there are useful functions which only work in the
context of a `MonadPlus` f.e. [`mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad.html#v:mfilter)
which filters a `MonadPlus` with a predicate:

```
λ> mfilter odd (Just 1)
Just 1
λ> mfilter odd (Just 2)
Nothing
λ> mfilter odd (Nothing)
Nothing
```


## Exercises

1. Implement the `isAliceOrBobOrCharly` example from the beginning in terms of
   our new `Parser i a` type.

   So you come up with a parser:

   ```haskell
   parseAliceBobCarly :: Parser Char String
   parseAliceBobCarly = ...
   ```

   You are allowed to utilize functions from `Data.List`

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

   As mentioned above there is a very close realtionship between `Alternative` and
   the `Monoid` typeclass which provides the semantics of _concatination_ of
   elements of a list using  `Monoid`s `mappend`
   named [`mconcat`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:mconcat).
   The module `Data.Foldable` provides a useful function with the same semantics
   on `Alternative` named
   [`asum :: (Foldable t, Alternative f) => t (f a) -> f a`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Applicative.html#v:asum). With that we could write the the function above also as:

   ```haskell
   parseAliceBobCarley' :: Parser Char String
   parseAliceBobCarley' = asum [ parseString "alice", parseString "bob", parseString "charly"]
   ```

   <hr />
   </details>

2. To be able to give the user better information about what actually went wrong
   in case of parser failure (parser didn't match? parser reached end of input?)
   replace the inner the `Maybe a` with a `Either err a` and fix class instances:

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

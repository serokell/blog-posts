<!--
* generic introduction
* say there are four parts for the article
  1. implement custom parser combinator library
  2. megaparsec tutorial
  3. qq
-->

Welcome! If you are reading this, you likely have decided to take on the journey of learning parser combinators. We hope this article will make your adventure smoother and hopefully give you a strong foundation for writing your grammars.

This article is composed of three major parts. In the first part, we will implement a small parser combinator library from scratch, which should hopefully help to give a feeling of how industrial-strength parsing combinators work. In the second part, we will learn how to use the Megaparsec library to implement a parser for S-expressions. Finally, as a bonus, we will use the power of Template Haskell to implement a quasi-quoter for our parser.

## What is a parser combinator and why use them?

<!--
* briefly explain what it is and how it works
* briefly explain their advantages (e.g.: simple to use, idiomatic, a lot of documentation)
* maybe compare with other techniques, like parser generators
  * maybe explain pros and cons
-->

In 2001, Daan Leijen and Erik Meijer published a paper titled [Parsec: Direct Style Monadic Parser Combinators For The Real World](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf), describing the [`parsec`](https://hackage.haskell.org/package/parsec) library, whose design consequently influenced various others, such as [`megaparsec`](https://hackage.haskell.org/package/megaparsec), [`attoparsec`](https://hackage.haskell.org/package/attoparsec), [`trifecta`](https://hackage.haskell.org/package/trifecta), and even libraries outside the Haskell ecosystem, such as [`NimbleParsec`](https://hexdocs.pm/nimble_parsec/NimbleParsec.html) for Elixir, [`parsec`](https://pythonhosted.org/parsec/) for Python, [`FParsec`](https://www.quanttec.com/fparsec/) for F#, among others.

Parser combinators are known to be simple to use without requiring external tools or too many concepts to learn. That is, they are ordinary Haskell constructors that can easily be combined and returned with other parsers because of their nature as functions. This, in turn, makes them idiomatic to use, and being a popular choice among Haskellers, their ecosystem is pretty developed. You should have no trouble finding tutorials and documentation on how to use them.

It's important to notice that parser combinators also have their flaws. Parser combinators may be implemented as either LL(1) or LL(∞) parsers, which, elaborating on the points described in Kirill Andreev's [How to Implement an LR(1) Parser](https://serokell.io/blog/how-to-implement-lr1-parser):
1. Don't support left recursion. That is to say, if there is a rule `foo = foo *> bar`, you might need to refactor your grammar otherwise it will loop forever.
2. Don't resolve conflicts, which we will see as an example later in this article. This means the ordering of rules can influence the output, and you need to be careful when defining rules that can consume the same input.
    * In an LL(1) implementation, it will pick the first alternative that maches the input.
    * In an LL(∞) implementation, it will try the longest possible match, which may be very inneficient.
3. Many parser combinator libraries will backtrack, which will also be exemplified later, meaning that you might need to be careful to avoid performance penalties.
4. The backtracking mechanism may, in turn, lead to exponential time complexities.

This shouldn't, however, discourage you from their use, as their benefits outweigh the drawbacks. Ultimately, the parsing library of your choice should be chosen based on your needs, and parser combinators are a good fit for most use cases.

## Implementing a simple parser combinator from scratch

<!--
* make an analogy: there is a tape, and we back move forward (parsed) or
  backward (failed) on this tape.
* datatype to parse String
* satisfy and instances for Monad and Alternative
* maybe explain what Alternative is, since beginners may not be familiar with it
-->

Before we delve into `megaparsec`, it may be interesting to implement our parser combinator from scratch. We will make a simple one that should hopefully help you understand how parser combinators work.

If you instead prefer to jump right into the action, then skip to the [Megaparsec tutorial](#megaparsec-tutorial).

### Implementation

We will begin by importing the appropriate definitions in a new file called `Parser.hs`:

```hs
module Parser where

import Control.Applicative (Alternative (..))
import Data.List (nub)
```

We will create two data structures that represent our parser.

The first of them represents some error that may happen while parsing. For now, let's have three errors: one that is raised when we are expecting input but there is nothing to consume (`EndOfInput`), one when we find an unexpected character (`Unexpected`), and one for any error messages that the user might want to raise (`CustomError`).

Our second structure is the actual parser. It's represented as a function that takes some input and either returns a list of errors, if `Left`, or the result of parsing the input together with the rest of the input (that was not parsed), if `Right`.

```hs
data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }
```

This is the meaning of each of the type variables:
* `i`: The input stream. For most cases, we will use `Char`, as `[Char]` is the same as `String`. It's interesting to notice that keeping this as a type variable allows us to parse things that are not strings as well.
* `e`: The type of custom error messages. If we don't have those, we may use `Void` instead.
* `a`: The result of our parsing function. It represents the structure parsed from our consumed input.

Let's begin by creating the most primitive function: `satisfy`. It tests the current character with a predicate, and if it succeeds, it will advance the parser and return the consumed character.

We must take care that we are not at the end of the input, however, in which case we should fail.

```hs
satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [Unexpected hd]
```

It's also common to define a function that only succeeds if the input strictly matches some given element.

```hs
char :: Eq i => i -> Parser i e i
char i = satisfy (== i)
```

If you load this in GHCi, you should now be able to test it:
```hs
>>> :l Parser

>>> runParser (char 'h') "hello"
Right ('h', "ello")

>>> runParser (char 'h') "greetings"
Left [Unexpected 'g']

>>> runParser (char 'h') ""
Left [EndOfInput]
```

Let's now create `Monad` and `Alternative` instances for our parser. For educative purposes, we will also explicitly implement `Functor` and `Applicative` instead of `fmap = liftM` and `(<*>) = ap`.

We start with `Functor`. Since we map over the `a` type variable obtained in the case of a successful parse, we can only apply `f` in case of `Right`.

```hs
instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)
```

Since `Either` is a monad, we can use do-notation. Here's an implementation that's a little shorter: 

```hs
instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)
```

Next up is `Applicative`. A good intuition behind `Applicative`s is that `pure` represents an identity element, while `<*>` distributes one argument over the other. Think of this like a multiplication, where `0 * a = a * 0 = 0`, while `1 * a = a * 1 = a`.

To illustrate this, observe this table:

| |Left parser failed|Left parser succeeded|
|-|-|-|
|Right parser failed|Error from left parser|Error from right parser|
|Right parser succeeded|Error from left parser|Success|

Note that even if both parsers fail, we only want the error from the first one, as we execute the second one only if the first one succeeded.

This is due to the `Either` part of the parser, where `Left` is like `0` and `Right` is like 1. `pure` represents our `1`, so it should create a parser that always succeeds without consuming any input.

And similarly to `Either`, we want `<*>` to fail in case any of the operands result in a failed parse given some input. This implementation tries to run the left operand, which consumes some input and produces a function as well as the unconsumed input. The rest of the input is given to the right operand, and in case of success, it applies the function to the argument.

```hs
instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input ->
    case f input of
      Left err -> Left err
      Right (f', rest) ->
        case p rest of
          Left err -> Left err
          Right (output, rest') -> Right (f' output, rest')
```

Or with `Either`'s monad instance:

```hs
instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')
```

In our `Monad` instance, we set `return = pure` as usual. Our bind is more interesting: we try to run the given parser with the input, and if it succeeds, we give the produced output (of type `a`) to our continuation `k`, which produces a parser that is executed with the remainder of the output.

```hs
instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) ->
        let
          Parser p' = k output
        in
        p' rest
```

Again, using the fact that `Either` is a monad, we can simplify this implementation:

```hs
instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest
```

With our `Applicative` and `Monad` instances, we are now able to **parse sequentially**. For example, we defined `char` above to let us parse a single character, but what if we wanted to parse many of them?

```hs
string :: Eq i => [i] -> Parser i e [i]
string = traverse char
```

If `traverse` is new to you, it's also equivalent to these definitions:

```hs
string :: Eq i => [i] -> Parser i e [i]
string [] = pure []
string (x : xs) = (:) <$> char x <*> string xs
```

Or even in terms of `Monad`:

```hs
string [] = return []
string (x : xs) = do
  y <- char x
  ys <- string xs
  return (y : ys)
```

Let's try it in GHCi:

```hs
>>> runParser (string "Haskell") "Haskell"
Right ("Haskell","")

>>> runParser (string "Haskell") "Halloween"
Left [Unexpected 'l']
```

Finally, our last instance: `Alternative`. This may be new for you, so I will explain. First, take a look at its definition, which can be found in `Control.Applicative`:

```hs
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

I mentioned that `pure` acts as `1`, which is a parser that always succeeds without consuming input. `empty` works like the converse (or `0`), that is a parser that always fails without consuming input.

For `(<|>)`, we want to implement it such that **the first parser to succeed is used**. That is, we first try the left operand, and failing that, we try the second one. The table below shall guide you:

| |Left parser failed|Left parser succeeded|
|-|-|-|
|Right parser failed|Errors from both parsers|Left parser|
|Right parser succeeded|Right parser|Left parser|

Here is an implementation of `Alternative`. We constrain `i` and `e` with `Eq` so we can use `nub` to remove duplicated errors. Note that this is not the best or most efficient way to merge errors, but for our purposes, it's good enough.

```hs
instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)
```

Finally, let's test it!

```hs
>>> runParser (string "hello" <|> string "greetings") "hello, world"
Right ("hello",", world")

>>> runParser (string "hello" <|> string "greetings") "greetings, world"
Right ("greetings",", world")

>>> runParser (string "hello" <|> string "greetings") "bye, world"
Left [Unexpected 'b']

>>> runParser (empty <|> pure ()) ""
Right ()

>>> runParser (pure () <|> empty) ""
Right ()
```

We can see our errors composing with a more complicated expression:

```hs
>>> runParser ((string "hello" *> string ", globe") <|> string "greetings") "hello, world"
Left [Unexpected 'w',Unexpected 'h']
```

Hopefully, this should give you an intuition of how parser combinators work. Popular parser combinator libraries are more complex than this, as they tend to keep track of extra state for better error messages, optimization, information about line and column, etc., but the overall operations between them are similar.

The complete code for this section with solutions to the exercises below can be found [here](https://gist.github.com/heitor-lassarote/3e7314956e86b8227f6f6040e69aca9d).

### Exercises

1. Change `char` so that it returns errors such as `Left [Expected 'h' 'g']` ("Expected 'h', but got 'g'").

<details>
  <summary>Solution</summary>
Begin by creating a new error:

```hs
data Error i e
  = ...
  | Expected i i
```

Next, we will refactor the code originally in `satisfy` into `token`, just so we avoid duplicating code. Then, rewrite `satisfy` to use this new function, and rewrite `char` to make use of it as well.

The important part is simply that we change the error message.

```hs
token :: (i -> Error i e) -> (i -> Bool) -> Parser i e i
token mkErr predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [mkErr hd]

satisfy :: (i -> Bool) -> Parser i e i
satisfy = token Unexpected

char :: Eq i => i -> Parser i e i
char i = token (Expected i) (== i)
```

In GHCi you should see:

```hs
>>> parse (char 'h') "bye"
Left [Expected 'h' 'b']
```
  <hr>
</details>

2. Create an `eof :: Parser i e ()` function which succeeds only if we have arrived at the end of the input.
    * `runParser (string "hello" <* eof) "hello"` should return `Right ("hello", "")`
    * `runParser (strung "hello, world" <* eof) ""` should return `Left [ExpectedEndOfFile ',']` ("Expected EOF, but got ','")

<details>
  <summary>Solution</summary>
First, create a new error:

```hs
data Error i e
  = ...
  | ExpectedEndOfFile i
```

Now create a function that returns `Right` only if there is no more input, and an error otherwise.

```hs
eof :: Parser i e ()
eof = Parser $ \input ->
  case input of
    []     -> Right ((), [])
    hd : _ -> Left [ExpectedEndOfFile hd]
```

Trying it out in GHCi:

```hs
>>> parse (string "bye" <* eof) "bye"
Right ("bye","")

>>> parse (string "bye" <* eof) "byebye"
Left [ExpectedEndOfFile 'b']
```
  <hr>
</details>

3. Change the definition of `Parser` so it contains a `[i] -> Offset -> (Either [Error i e] (Offset, a, [i]))`, where `type Offset = Int`. The offset indicates how many positions we have parsed.
    * You may also change `Error` to store an offset so that you can indicate where an error occurred.
    * `runParser (string "hey") 0 "hi"` should return `Left [Error {erOffset = 1, erError = Expected 'e' 'i'}]` ("Unexpected 'e', but got 'i', at position 1")

<details>
  <summary>Solution</summary>
The solution is mostly mechanically replacing definitions where errors occurred.

First, we rename `Error` to `ErrorType`, and create a new `Error` type:

```hs
data Error i e = Error
  { erOffset :: Offset
  , erError :: ErrorType i e
  } deriving (Eq, Show)

data ErrorType i e
  = ...
```

If you've done the previous exercises, then `token` may be changed now to take `offset` as input to the parser's function, and return `offset + 1` on success. If an error occurred instead, we now construct `Error` with the given input. Our `mkErr` function should not take an `ErrorType` instead of `Error`.

```hs
token :: (i -> ErrorType i e) -> (i -> Bool) -> Parser i e i
token mkErr predicate = Parser $ \input offset ->
  case input of
    [] -> Left [Error offset EndOfInput]
    hd : rest
      | predicate hd -> Right (offset + 1, hd, rest)
      | otherwise    -> Left [Error offset $ mkErr hd]
```

If you haven't done the previous exercises, then change `satisfy` in a similar manner, taking `offset` as input and returning `offset + 1`, and error `Error offset $ Unexpected hd` on the error.

For the remainder of the functions, change the parser function to also take `offset` as an argument, and just return it unaffected. Take care to chain these offsets as required. For example, this is the new definition of `Applicative`:

```hs
instance Applicative (Parser i e) where
  pure a = Parser $ \input offset -> Right (offset, a, input)

  Parser f <*> Parser p = Parser $ \input offset ->
    case f input offset of
      Left err -> Left err
      Right (offset', f', rest) ->
        case p rest offset' of
          Left err -> Left err
          Right (offset'', output, rest') -> Right (offset'', f' output, rest')
```

Finally, in `parse`, use `0` as the initial offset.

Keep in mind it's also possible to operate with line and column when you know you have a function operating in characters.

```hs
>>> parse (string "hey") "hi"
Left [Error {erOffset = 1, erError = Expected 'e' 'i'}]
```

The reader may also choose to instead refactor the parser type to use a custom-defined `State` type that contains the input stream and offset. Many improvements are possible, but we choose to keep it simple for educational purposes.
  <hr>
</details>

### What's next?

In this section, we've learned how to make a simple parsing combinator library. There are still various improvements that can be done. To name a few of them, we can implement more instances, such as `Semigroup` and `Monoid`, `IsString`, `MonadFail`, `MonadPlus`, among others. Error messages can be improved by better keeping track of location (such as line and column), supporting more types such as `Text` and `ByteString`, pretty-printing error messages, etc.

We will conclude this part here, and if you are interested, proceed to the next part where Megaparsec, an industrial-strength parser combinator library, will be introduced with a more practical application than our simple examples in this section.

## Megaparsec tutorial

<!--
* explain why we chose megaparsec
* parse S-exps
* make a small format for data parsing, no programming constructs
  * equivalent to JSON: objects, lists, null, numbers, booleans, strings
-->

We will now take a look at a practical application of parser combinators: parsing [S-expressions](https://en.wikipedia.org/wiki/S-expressions). Hopefully, this example will give you a solid foundation for creating parsers for your grammars.

Megaparsec is the go-to parser for industrial projects, so we will also use it in our tutorial. If you wish to see how it compares to other parser combinator libraries in Haskell, you can read its [`README.md`](https://github.com/mrkkrp/megaparsec#comparison-with-other-solutions).

Keep in mind you should choose the parser library that better suits your needs. If you plan to parse machine-written data, for example, then Attoparsec may be a better choice. Its interface is similar to Megaparsec's, so the knowledge you will learn here will also be useful.

Without any further ado, we will now present how to use Megaparsec to parse some S-expressions.

### Representing S-expressions

To begin, create a file called `SExp.hs`. Also, make sure to get the [`megaparsec`](https://hackage.haskell.org/package/megaparsec) package.

We will use the following data structure to represent various S-expressions. Gradually, we will write a parser that represents each of these cases.

```hs
module SExp where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Identifier = Identifier
  { getId :: String
  } deriving (Show)

data SExp
  = SSExp    SExp [SExp]  -- (foo 0 "hello" bar), (bar (baz 1)), (foo)
  | SInteger Integer  -- 42
  | SString  String  -- "hello, world"
  | SBool    Bool  -- false, true
  | SId      Identifier  -- foo
  deriving (Show)
```

We also need an alias for the type which `megaparsec` uses.

```hs
type Parser = Parsec
  -- The type for custom error messages. We have none, so use `Void`.
  Void
  -- The input stream type. Let's use `String` for now, but for
  -- better performance, you might want to use `Text` or `ByteString`.
  String
```

### Booleans

Let's start with the simplest of the parsers: `SBool`. We use `<|>` which tries one parser, and failing that, will try the other one.

```hs
bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true"
```

We will also make a parser to represent our `SExp`-specific variants. As we progress, we will add more parsers to this list.

```hs
atom :: Parser SExp
atom = choice
  [ SBool <$> bool
  ]
```

Here we use `choice`, which is the same as running `<|>` between each element of the list. `choice [a, b, c] = a <|> b <|> c`.

It's important to notice that `choice`, as well as many other useful combinators, are part of the [`parser-combinators`](https://hackage.haskell.org/package/parser-combinators-1.3.0) package, which Megaparsec conveniently re-exports for us. Should you fail to find a specific function in Megaparsec's documentation, chances are that it's defined in `parser-combinators` instead.

Now, fire up GHCi and test that we can parse it! `megaparsec` provides the `parseTest` function which allows us to conveniently test our parsers.

```hs
>>> parseTest atom "false"
SBool False

>>> parseTest atom "hey"
1:1:
  |
1 | hey
  | ^^^
unexpected "hey"
expecting "false" or "true"
```

### Integers

To parse integers, one strategy is to parse as many digits (chars whose value is between `'0'` and `'9'`) as possible. This will give us a list of characters, which we can then `read`.

For this parser, we use the `some` function, which tries to run a given parser **1 or more times**. `numberChar` is exported from `Text.Megaparsec.Char` and parses any character that matches a digit. In regex, this is equivalent to `[0-9]+`. This will return us a `[Char]` (aka `String`), which we can `read` to make it into an `Integer`.

```hs
integer :: Parser Integer
integer = read <$> some numberChar
```

We now change our `atom` to also include integers.

```hs
atom :: Parser SExp
atom = choice
  [ SBool <$> bool
  , SInteger <$> integer
  ]
```

Let's also test it in GHCi:

```hs
>>> parseTest atom "42"
SInteger 42

>>> parseTest atom "hey"
1:1:
  |
1 | hey
  | ^^^
unexpected "hey"
expecting "false", "true", or numeric character
```

Although the error message is good, wouldn't it better if it said "integer" instead of "numeric character"? For this, we can use **labels**, which allows us to give a name to a parser. To do this, we can simply change our parser like this:

```hs
integer :: Parser Integer
integer = label "integer" $ read <$> some numberChar
```

If you prefer, it's also possible to use `<?>`, which does the same as `label`.

```hs
integer :: Parser Integer
integer = read <$> some numberChar <?> "integer"
```

And now the error message should be slightly improved:

```hs
>>> parseTest atom "hey"
1:1:
  |
1 | hey
  | ^^^
unexpected "hey"
expecting "false", "true" or integer
```

You can also use `label "boolean"` in our `bool` parser to change the message to `expecting boolean or integer`.

**Exercise**: Can you change this parser so it also accepts negative integers?

<details>
  <summary>Solution</summary>
One possible solution using `<|>`:

```hs
integer :: Parser Integer
integer = label "integer" $ read <$> (some numberChar <|> ((:) <$> char '-' <*> some numberChar))
```

However, later in the tutorial, we will see an even better way that uses Megaparsec's own mechanisms.
  <hr>
</details>

### Strings

To parse a string, the idea is to find everything that is between double-quotes. This approach has a weakness, though: we won't be able to parse escaped quotes. Later in this tutorial, we will present a solution to escaped characters using Megaparsec's lexing mechanisms.

To achieve this, we will use two new functions: `between` and `takeWhileP`. Using `between open close x` is equivalent to `open *> x <* close`, while `takeWhileP` will read as many characters as possible that satisfy some predicate.

Note that `takeWhileP` also accepts, optionally, a label for each character that it tries to read. For now, we will leave it as `Nothing`.

```hs
str :: Parser String
str = label "string" $ between (char '"') (char '"') (takeWhileP Nothing (/= '"'))
-- Or:
--- str = label "string" $ char '"' *> takeWhileP Nothing (/= '"') <* char '"'
```

In `atom`, include `SString <$> str`.

```hs
>>> parseTest atom "\"hey\""
SString "hey"
```

### Identifiers

An identifier will be like a variable in a programming language. We can choose any naming convention for such a thing, but here I chose the one that is frequently used in C-like languages: the first letter must be a letter or an underscore, while the remainder may be a letter, digit, or underscore.

Here we use the `many` function, which is very similar to the `some` function discussed before. The difference is that `many` tries to run the given parser **0 or more times**. In regex, our `identifier` parser is equivalent to `[a-zA-Z_][a-zA-Z0-9_]*`.

```hs
identifier :: Parser Identifier
identifier = label "identifier" $ do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  pure $ Identifier $ first : rest
-- Or:
-- identifier = Identifier <$> label "identifier" ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
```

Now include `SId <$> identifier` to `atom`.

**Warning**: When parsing with `<|>` or `choice`, **the order of the parsers matters**. Take this as an example:

```hs
>>> parseTest (SBool <$> bool <|> SId <$> identifier) "true"
SBool True

>>> parseTest (SId <$> identifier <|> SBool <$> bool) "true"
SId (Identifier {getId = "true"})
```

In the first case, we parse a boolean before an identifier, which causes `true` to be matched by `bool`. However, in the second example, using `identifier` before `bool` caused `true` to be recognized as an identifier. You should be careful while considering such things. In most cases, you can assume that parsers are greedy. Notice that in the second case, the `bool` parser will never be executed.

In other words, make sure that you parse a boolean in `atom` before you parse an identifier.

### S-expressions

Finally, let's move on to our last parser, which parses S-expressions themselves, such as `(foo 42 "hey")` or `(bar)`.

First, we will do it the naïve way, which may come as the first natural solution to someone just learning Megaparsec, and then we will do it in a better manner using extra tools offered by Megaparsec.

#### S-expressions: the awkward way

The idea here is to parse an atom, followed by 0 or more atoms. Of course, this means that we want to use `many` to parse such atoms. Additionally, an S-expression may be surrounded by parenthesis, so we can use `between` for such a task.

This parser, however, is not as simple as before, but let's first do it the wrong way (which won't work properly), analyze the problem, and come up with a solution to it.

```hs
sexp :: Parser (SExp, [SExp])
sexp = label "S-expression" $ between (char '(') (char ')') ((,) <$> atom <*> many atom)
-- Or:
-- sexp = label "S-expression" $ char '(' *> ((,) <$> atom <*> many atom) <* char ')'
```

Note that `(,) <$> atom <*> many atom` is pretty similar to `some atom`. [`parser-combinators`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators-NonEmpty.html#v:some) exports a non-empty `some` that you can use instead, and have the parser return `Parser (NonEmpty SExp)`. Megaparsec exports the ordinary list version instead, so if you want to use this one, you'll need to import it manually and include a dependency on `parser-combinators`.

After adding it to `atom` with `uncurry SSExp <$> sexp`, we can test it in GHCi:

```hs
>>> parseTest atom "(foo)"
SSExp (SId (Identifier {getId = "foo"})) []
```

Ok, looks good so far. What if we try to apply an argument to `foo`?

```hs
>>> parseTest atom "(foo bar)"
1:5:
  |
1 | (foo bar)
  |     ^
unexpected space
expecting ')', '_', S-expression, alphanumeric character, boolean, identifier, integer, or string
```

Uh-oh. We can't parse spaces, as the error makes evident. Either we close parenthesis (stop parsing this S-expression), continue the definition of the identifier (`_` or `alphanumeric character`), put another atom (`boolean, identifier, integer, or string`), or another S-expression (by opening parenthesis).

One solution that could come to one's mind is to use the function `sepBy`, which takes a parser and a separator, and use it as ``atom `sepBy` space``. In this case, `space` is a parser defined by Megaparsec that skips zero or more spaces.

```hs
>>> parseTest (between (char '(') (char ')') $ SSExp <$> atom <*> (space *> atom `sepBy` space)) "(foo bar)"
SSExp (SId (Identifier {getId = "foo"})) [SId (Identifier {getId = "bar"})]
```

Of course, this may raise other questions: what if we have a space character before or after the opening and closing parenthesis?

While explicitly skipping spaces works, the [`megaparsec` documentation](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char-Lexer.html) already offers a solution for this kind of problem, namely, a lexer. Indeed, we will see that some of our previous definitions could have been written more elegantly with the help of this module.

The important bit for us now is this:

> Parsing of white space is an important part of any parser. We propose a convention where **every lexeme parser assumes no spaces before the lexeme and consumes all spaces after the lexeme**; this is what the [`lexeme`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char-Lexer.html#v:lexeme) combinator does, and so it's enough to wrap every [`lexeme`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char-Lexer.html#v:lexeme) parser with lexeme to achieve this. Note that you'll need to call [`space`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char-Lexer.html#v:space) manually to consume any white space before the first lexeme (i.e. at the beginning of the file).

#### S-expressions: the elegant way

The first thing you should do is import the appropriate lexing module. Megaparsec recommends to import this module qualified.

```hs
import qualified Text.Megaparsec.Char.Lexer as L
```

Now we need to define what is whitespace. Until now, with the usage of `space`, we have considered that it may be a space, tab, newline, carriage return, form feed, or vertical tab. Megaparsec can go beyond that and also consider line comments (such as `-- comment`) and block comments (such as `{- comment -}`) as whitespace. Since we get those for free, what about using `;;` for line comments and `/* */` for block comments?

Begin by defining `skipSpace`. We use the `space` helper function from the lexer module, which expects a function to parse 1 or more spaces, a line comment, and a block comment.

```hs
skipSpace :: Parser ()
skipSpace = L.space
  -- Like `space`, but skips 1 or more space characters.
  space1
  -- Skip from ;; until a newline.
  (L.skipLineComment ";;")
  -- Skip from /* until */. There is also `skipBlockComment`, but it doesn't handle nested comments.
  (L.skipBlockCommentNested "/*" "*/")
```

And now define a `lexeme` parser, which indicates how to consume space after the given parser.

```hs
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace
```

Now you can use `lexeme` in each of the parsers, and they will automatically skip whitespace after their definitions. Note that we also use `lexeme` before our first opening parenthesis, so we can skip space just after the `(`.

```hs
sexp :: Parser (SExp, [SExp])
sexp = label "S-expression" $ lexeme $
  between (lexeme (char '(')) (char ')') ((,) <$> atom <*> many atom)
```

You may add `lexeme` to other parsers where it's desirable to skip whitespace after the rule. For instance:

```hs
integer :: Parser Integer
integer = label "integer" $ lexeme $ read <$> some numberChar
```

**Note**: The reader should do the same for `str`, `bool`, and `identifier`. Alternatively, adding `lexeme` to `atom` will also work.

And now we can parse S-expressions properly:

```hs
>>> parseTest atom "(  foo  /* 8-) */  bar  ;; look mom, comments! \n  )"
SSExp (SId (Identifier {getId = "foo"})) [SId (Identifier {getId = "bar"})]
```

### Our main parser function

Finally, let's make a function that solves the last two outstanding problems.

For the first one, as mentioned by the Megaparsed documentation, `lexeme` will skip whitespace after a parser, but we also need to deal with whitespace at the beginning of the file.

For the second one, we want to parse everything until we find an `eof` since our parser will currently ignore anything after a complete atom. The two defects are exemplified below:

```hs
>>> parseTest atom "  42"
1:1:
  |
1 |   42
  | ^^^^
unexpected "  42"
expecting S-expression, boolean, identifier, integer, or string

>>> parseTest atom "42 foo bar"
SInteger 42
```

We will create a function that takes our input string, and either returns an error message or the actual result. Such function "bootstraps" our parser with `between skipSpace eof atom`, which will solve the two defects. It will also serve as an auxiliary function that can be called by client code, rather than `parseTest` which is more useful for working in GHCi.

```hs
parseSExp :: String -> Either String SExp
parseSExp input =
  let
    outputE = parse
      -- Skip any whitespace at the beginning, expect the end of input after the atom.
      (between skipSpace eof atom)
      -- Name of the source file, you can give it any name you want. I leave it blank for now.
      ""
      -- The actual string to be parsed
      input
  in
  -- If we get Left, it will be an `ParseErrorBundle`, let's pretty print it for now.
  -- If we get Right, do nothing.
  case outputE of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output
```

Now in GHCi:

```hs
>>> case parseSExp "   42" of { Left e -> putStrLn e; Right r -> print r }
SInteger 42

>>> case parseSExp "42 foo" of { Left e -> putStrLn e; Right r -> print r }
1:4:
  |
1 | 42 foo
  |    ^
unexpected 'f'
expecting end of input
```

### One more case: `Double`

There is one more **caveat** that Megaparsec users should know about: backtracking. We show a somewhat contrived but realistic example. To illustrate this, let's add one more case to our `SExp`:

```hs
data SExp
  = ...
  | SDouble  Double  -- 42f, 3.1415f
```

We will parse it as such: first, some digits, optionally followed by a `.` and some more digits, and an `f` at the end. In regex, this is equivalent to `[0-9](\.[0-9])?f`. We achieve the optional part using the `optional` function, which returns a `Parser (Maybe a)`.

```hs
double :: Parser Double
double = label "double" $ lexeme $ read <$> do
  left <- some numberChar
  rightM <- optional $ do
    char '.'
    some numberChar
  char' 'f'  -- Like char, but case-insensitive
  pure $ case rightM of
    Nothing -> left
    Just right -> left ++ "." ++ right
```

Now, we add it to `atom`: `SDouble <$> double`. Depending whether you added `double` before or after `integer`, the result may be surprising:

<table>
  <tr>
    <td>Parser \ Position</td>
    <td>`double` before `integer`</td>
    <td>`double` after `integer`</td>
  </tr>
  <tr>
    <td>
      <pre lang="hs">parseTest atom "42"</pre>
    </td>
    <td>
      <pre>
1:3:
  |
1 | 42
  |   ^
unexpected end of input
expecting '.', 'F', 'f', or numeric character
      </pre>
    </td>
    <td>
      <pre lang="hs">SDouble 42.0</pre>
    </td>
  </tr>
  <tr>
    <td>
      <pre lang="hs">parseTest atom "42f"</pre>
    </td>
    <td>
      <pre lang="hs">SInteger 42</pre>
    </td>
    <td>
      <pre lang="hs">SInteger 42</pre>
    </td>
  </tr>
</table>

So it only got one case correctly. To fix it, one solution would be to use `try`, which allows Megaparsec to do as much backtracking as it wants.

```hs
-- Note: Other cases omitted for brevity.
atom :: Parser SExp
atom = choice
  [ SDouble <$> try double
  , SInteger <$> integer
  ]
```

And it should fix the problem. However, this is not ideal in terms of performance, as it will cause Megaparsec to consume the entirety of the `double` to later find that there is no `f`, backtrack, and finally try again with an `integer`.

In such a situation this is trivial, but care must be taken in more complex examples, for example, what if we wanted to have `(foo bar)` as well as tuples such as `(foo bar, baz quz)`? The S-expression could be very long and lead to exponential complexity in the worst-case scenario. Recently, a very similar problem [was fixed](https://gitlab.com/morley-framework/morley/-/merge_requests/971) in one of our projects.

As we described in the introduction of this tutorial, it's important to notice Megaparsec normally operates as a LL(1) parser, meaning it tries to look at only one character ahead when executing parsers in an alternative. Using `try` allows us to consume an arbitrary number of characters before backtracking, allowing it to be used as a LL(∞) parser.

In this situation, the best thing to do is to _refactor the grammars to share common parts_. The code above could be written without `try` as such:

```hs
numeric :: Parser SExp
numeric = label "number" $ lexeme $ do
  -- Left side before . or f
  left <- some numberChar

  -- .0f or f
  rightM <- optional $ choice
    [ do
        char '.'
        right <- some numberChar
        char' 'f'
        pure $ left ++ "." ++ right
    , do
        char' 'f'
        pure left
    ]

  -- If we had the right side, it's a double, otherwise an integer.
  pure $ case rightM of
    Nothing -> SInteger $ read left
    Just right -> SDouble $ read right
```

And in `atom`, we simply use `numeric` instead of the two previous parsers.

### Rewriting with Megaparsec's lexer

A lot of the things could be written using the lexer module from Megaparsec. In this section, we will present the simplified versions for functions that can be improved.

Because we will use [`floatingOrInteger`](https://hackage.haskell.org/package/scientific-0.3.7.0/docs/Data-Scientific.html#v:floatingOrInteger), you should include the [`scientific`](https://hackage.haskell.org/package/scientific-0.3.7.0) package and import it:

```hs
import Data.Scientific
```

And here is our rewritten parser. Note that our new `numeric` has a quirk: `1.0` is parsed as an integer by Megaparsec. A resolution to this is left as an exercise to the reader.

```hs
-- signed takes a parser on how to skip space after the sign.
integer :: Parser Integer
integer = label "integer" $ lexeme $ L.signed skipSpace L.decimal

double :: Parser Double
double = label "double" $ lexeme $ L.signed skipSpace L.float <* char' 'f'

-- charLiteral also takes escaped characters into question.
str :: Parser String
str = label "string" $ lexeme $ char '"' *> manyTill L.charLiteral (char '"')

numeric :: Parser SExp
numeric = lexeme $ do
  value <- L.signed skipSpace L.scientific
  case floatingOrInteger value of
    Left d -> SDouble d <$ char' 'f'
    Right i -> do
      f <- optional $ char' 'f'
      pure $ case f of
        -- Note: 1.0 is an integer, but 1.1 is a parser error.
        Nothing -> SInteger i
        Just _ -> SDouble $ fromIntegral i
```

Note the changes to `str` as well. `manyTill` will run `L.charLiteral` taking characters, including escaped characters until the matching `"` is found.

As a bonus, we got various improvements:
1. No need to `read` values.
2. We can use `signed` to parse negative numbers.
3. We can parse escaped characters in strings.

The functions for `integer` and `double` were kept for reference, even though `numeric` is used instead.

Now let's check it in GHCi:

```hs
>>> parseTest atom "(foo -42 \"with \\\" now!\")"
SSExp (SId (Identifier {getId = "foo"})) [SInteger (-42),SString "with \" now!"]
```

And I hope that with this you can better understand how parser combinators work. Now get to hacking your languages!

The complete code for this part can be found [here](https://gist.github.com/heitor-lassarote/ea54d05aca2956efa29b47ebbf048cbd).

## Bonus: quasi-quotations

<!--
* show how to use QQ to parse in compile-time
-->

This section is better understood if you have some Template Haskell (TH) knowledge. If you haven't learned about it, then I **strongly** encourage you to check out our [Introduction to Template Haskell](https://serokell.io/blog/introduction-to-template-haskell) before trying out this section.

Note that this section is not necessary for our knowledge of parser combinators, and it's purely a bonus section on how to do more with our S-expressions.

For this part, you will need the [`template-haskell`](https://hackage.haskell.org/package/template-haskell) package.

Long story short, Template Haskell is a mechanism that allows GHC to evaluate and analyze code during compile-time. One such application is using `QuasiQuoter`s, which allows us to run custom parsers during compile time. Here we will use them for two purposes: to use S-expressions as Haskell expressions and to use S-expressions as Haskell patterns, both of which can be parsed during compilation.

We will create a new file called `TH.hs`, whose definition is given below.

```hs
module TH where

import Control.Monad ((<=<))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Q, dataToPatQ, liftData)

import SExp (SExp, parseSExp)

sexp :: QuasiQuoter
sexp = QuasiQuoter
  { quoteExp  = liftData <=< parseOrThrow
  , quotePat  = dataToPatQ (const Nothing) <=< parseOrThrow
  , quoteType = const $ fail "No S-expression parser for type"
  , quoteDec  = const $ fail "No S-expression parser for declarations"
  }
  where
    parseOrThrow :: String -> Q SExp
    parseOrThrow = either fail pure . parseSExp
```

Additionally, `liftData` and `dataToPatQ` assume that our parsed `SExp` implement the `Data` type class. We will change our `SExp` module to automatically derive this datatype for us. First, enable the `DeriveDataTypeable` language extension, so GHC can automatically implement `Data` for us.

You also need to import `Data` from `Data.Data`.

```hs
{-# LANGUAGE DeriveDataTypeable #-}

module SExp where

import Data.Data (Data)
-- The rest of the file goes here...
```

And now change `Identifier` and `SExp` to derive `Data`:

```hs
newtype Identifier = Identifier
  { getId :: String
  } deriving (Data, Show)

data SExp
  = SSExp    SExp [SExp]  -- (foo 0 "hello" bar), (bar (baz 1)), (foo)
  | SInteger Integer  -- 42
  | SString  String  -- "hello, world"
  | SBool    Bool  -- false, true
  | SId      Identifier  -- foo
  | SDouble  Double  -- 42f, 3.1415f
  deriving (Data, Show)
```

The basic contents of the `TH` module are summarized as follows:
* Each field of `QuasiQuoter` provides a `String` parser for a different Haskell syntax.
* `quoteExp` creates a Haskell expression.
  * This will allow us to write expressions such as `let mySExp = [sexp|(foo 42)|]`.
  * We use our standard Megaparsec parser, `parseSExp`, but fail if it can't parse (halting compilation). `liftData` then transforms our `SExp` into an `Exp`.
  * If this is your first time seeing `<=<`, it's just another way to compose monadic functions. `g <=< f` is the same as `\x -> g =<< f x`.
  * If you've read our previous TH tutorial, it creates an `Exp` just like `[e||]`.
* `quotePat` creates a Haskell pattern.
  * Example: `case mySExp of {[sexp|(foo 42)|] -> True; _ -> False}`.
  * Similarly to `quoteExp`, we use `dataToPatQ (const Nothing)` to transform our `SExp` into a `Pat`.
    * Confusingly, `liftData` is defined as `dataToExpQ (const Nothing)`.
    * The `const Nothing` can be replaced to handle type-specific cases. We are fine with the default behavior for now but we'll change it later.
  * Creates a `Pat`, like `[p||]`.
* `quoteType` creates a Haskell type.
  * Since we have no `SExp`, `SInteger` etc types matching our constructors, we will fail when someone tries to use our parser as types.
  * Creates a `Type`, like `[t||]`.
* `quoteDec` creates a Haskell declaration.
  * Similarly to `quoteType`, we won't define this one.
  * Creates a `Dec`, like `[d||]`.

Now enable the `QuasiQuotes` extension in GHCi and you should be able to test it. Notice how failing to parse instead causes GHCi to fail to interpret the given S-expression (compilation error).

```hs
>>> :set -XQuasiQuotes
>>> [sexp|(foo "bar" 42)|]
SSExp (SId (Identifier {getId = "foo"})) [SString "bar",SInteger 42]

>>> [sexp|no parenthesis|]
Out [19]:

<interactive>:1135:7: error:
    • 1:4:
  |
1 | no parenthesis
  |    ^
unexpected 'p'
expecting end of input

    • In the quasi-quotation: [sexp|no parenthesis|]
```

There are still two improvements we could do, however. The first one is that Megaparsec could use the actual Haskell location:

```hs
>>> let SInteger x = [sexp|.|] in x
(omitted for brevity, error in 1:1)
```

As for the second one, it would be much better if we could bind S-expression variables into Haskell ones, such as the following:

```hs
foo [sexp|(foo $x)|] = Just x
foo _                = Nothing
```

### Using Haskell locations

Let's change our parser runner a bit so it starts with the current line and column. We will create a new auxiliary `parseSExpWithPos`, which takes a `SourcePos` and uses it for the initial state. This function was created from our old `parseSExp`.

Additionally, we create a new `parseSExp` with the default position.

We now call `runParser'` instead of directly calling `parse`, as it allows us to give it an initial state and build such a state with the default settings. Keep in mind that we're getting into the internal behavior of Megaparsec here, so changes in the package can break this code.

```hs
parseSExp :: String -> Either String SExp
parseSExp = parseSExpWithPos (initialPos "")

parseSExpWithPos :: SourcePos -> String -> Either String SExp
parseSExpWithPos pos@(SourcePos file _line _column) input =
  let
    initState = State
      { stateInput = input
      , stateOffset = 0
      , statePosState = PosState
        { pstateInput = input
        , pstateOffset = 0
        , pstateSourcePos = pos
        , pstateTabWidth = defaultTabWidth
        , pstateLinePrefix = ""
        }
      , stateParseErrors = []
      }
    outputE = snd $ runParser'
      -- Skip any whitespace at the beginning, expect the end of input after the atom.
      (between skipSpace eof atom)
      -- The input state with the string to be parsed.
      initState
  in
  -- If we get Left, it will be a `ParseErrorBundle`, let's pretty print it.
  -- If we get Right, do nothing.
  case outputE of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output
```

Now we can change our `TH` modules. We will make new imports:

```hs
import Language.Haskell.TH.Syntax (Loc (..), Q, dataToPatQ, liftData, location)
import Text.Megaparsec (SourcePos (..), mkPos)

import SExp (SExp, parseSExpWithPos)
```

And we will change `parseOrThrow` as follows:

```hs
    parseOrThrow :: String -> Q SExp
    parseOrThrow input = do
      posTH <- location
      let pos = SourcePos
            (loc_filename posTH)
            (mkPos $ fst $ loc_start posTH)
            (mkPos $ snd $ loc_start posTH)
      either fail pure $ parseSExpWithPos pos input
```

With it, we now use `location` to query where the splice occurred and put it in the format that `Megaparsec` expects. Trying it in GHCi gives me:

```hs
>>> let SInteger x = [sexp|foo 42|] in x
<interactive>:1730:24: error:
    • <interactive>:1730:28:
     |
1730 | foo 42
     |
unexpected '4'
expecting end of input

    • In the quasi-quotation: [sexp|foo 42|]
```

For visualization, it may be better if we create a small test file:

```hs
module Test where

import TH (sexp)

testQQ = [sexp|foo 42|]
```

Compiling it results in:

```hs
Test.hs:5:16: error:
    • Test.hs:5:20:
  |
5 | foo 42
  |
unexpected '4'
expecting end of input

    • In the quasi-quotation: [sexp|foo 42|]
  |
5 | testQQ = [sexp|foo 42|]
  |                ^^^^^^^^
```

You may freely change how errors are printed by changing `errorBundlePretty` with a custom error printing function, but for this tutorial, this is good enough for us.

### Binding variables in patterns

As mentioned, it would be nice if we could bring S-expression bindings into the Haskell scope. For this, let's add a new term into our `SExp`:

```hs
data SExp
  -- other constructors omitted for brevity
  | STH String  -- $meta
```

We may now add a parser to extract it. Let's reuse our `identifier` for this:

```hs
th :: Parser String
th = label "TH variable" $ lexeme $ char '$' *> fmap getId identifier
```

Don't forget to add it to `atom` with `STH <$> th`.

We are now ready to add it to our `TH` module. First, we will need some new imports:

```hs
import Data.Typeable (Typeable, cast)
import Language.Haskell.TH.Lib (varP)
import Language.Haskell.TH.Syntax (Loc (..), Name, Q, dataToPatQ, liftData, location, mkName) -- no Loc and location if you've skipped the previous item

import SExp (SExp (STH), parseSExpWithPos)  -- or parseSExp if you've skipped the previous item
```

`Typeable` is a type class that allows us to try to `cast` some type into another. Here, we attempt a cast into `SExp`

We will add a new function to aid us:

```hs
sexp = -- ommited for brevity
  where
    antiQuoteVar :: Typeable a => a -> Maybe (Q Pat)
    antiQuoteVar var = case cast var of
      Nothing -> Nothing
      Just var' -> case var' of
        STH var'' -> Just $ varP $ mkName var''
        _other    -> Nothing
```

We can now replace `const Nothing` with `antiQuoteVar` in our quasi-quoter:

```hs
  , quotePat  = dataToPatQ antiQuoteVar <=< parseOrThrow
```

Now check whether it works:

```hs
>>> case [sexp|(foo 42)|] of
...  [sexp|(foo $answer)|] -> print answer
...  _ -> putStrLn "oops"
SInteger 42
```

It's also possible to give a meaning to `$answer` at the expression level by having it capture a Haskell variable instead. Let's change `TH` a bit to support it:

```hs
    antiQuoteVar :: Typeable a => (Name -> Q b) -> a -> Maybe (Q b)
    antiQuoteVar mkVar var = case cast var of
      Nothing -> Nothing
      Just var' -> case var' of
        STH var'' -> Just $ mkVar $ mkName var''
        _other    -> Nothing
```

And our `QuasiQuoter` now has:

```hs
  { quoteExp  = dataToExpQ (antiQuoteVar varE) <=< parseOrThrow
  , quotePat  = dataToPatQ (antiQuoteVar varP) <=< parseOrThrow
```

Don't forget to import `dataToExpQ`, which now replaces `liftData` as well as `varE`. We can now test our quasi-quoter again:

```hs
>>> let x = SInteger 42 in case [sexp|(foo $x)|] of
          [sexp|(foo $answer)|] -> print answer
          _ -> putStrLn "oops"
SInteger 42
```

And that's it! You now support quasi-quotations.

The complete code for the parser and quasi-quoter may be found [here](https://gist.github.com/heitor-lassarote/f784c16da80bcfc5271558ede51c70d9).

## Conclusion

<!--
* mention each topic discussed previously
* conclude why to use parser combinators
* the parser from scratch could use type families to handle other data (Text, ByteString), etc
-->

In this post, we've introduced the theory and a practical application of parser combinators. We've discussed what are they as well as their pros and cons, implemented a simple parser combinator from scratch, learned how to write an S-expression parser with Megaparsec, and created a simple quasi-quotation to consume input at compile-time.

Some improvements can still be made and they are left as an exercise to the reader. For example, the S-expression parser operates on `String`s, but Megaparsec also supports `Text` and `ByteString` as the input stream which should have better performance. On top of that, if the reader is familiar with type families (otherwise check out our article on [Type Families](https://serokell.io/blog/type-families-haskell)), they may try to improve our parser from scratch to also operate on more data types.

## Appendix

### Common parsing functions

Below we present some other useful functions and combinators you may want to use. We show their names, types, and comment about them.

|Name|Type|Documentation|
|-|-|-|
|[`many`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Applicative.html#v:many)|`Alternative f => f a -> f [a]`|Parse 0 or more occurrences of the given parser.|
|[`some`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Applicative.html#v:some)|`Alternative f => f a -> f [a]`|Parse 1 or more occurrences of the given parser.|
|[`option`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:option)|`Alternative m => a -> m a -> m a`|Parse 0 or 1 occurence of the given parser, with a fallback `a` if it fails.|
|[`optional`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:optional)|`Alternative f => f a -> f (Maybe a)`|Like `option`, but returns a `Maybe` instead.|
|[`sepBy`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:sepBy), [`sepBy1`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:sepBy1)|`Alternative m => m a -> m sep -> m [a]`|Parses various occurences of the given parser separated by the given separator. Functions prefixed with `1` parse 1 or more, while the ones without parse `0` or more.|
|[`sepEndBy`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:sepEndBy), [`sepEndBy1`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:sepEndBy1)|`Alternative m => m a -> m sep -> m [a]`|Like the `sepBy` variants, but additionally allow the separator to appear optionally at the end of the sequence.|
|[`endBy`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:endBy), [`endBy1`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html#v:endBy1)|`Alternative m => m a -> m sep -> m [a]`|Like the `sepEndBy` variants, but the separator at the end is not optional.|
|[`char`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char.html#v:char)|`(MonadParsec e s m, Token s ~ Char) => Token s -> m (Token s)`|Parses a single character matching the given char.|
|[`char'`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char.html#v:char-39-)|`(MonadParsec e s m, Token s ~ Char) => Token s -> m (Token s)`|Like `char`, but case-invariant.|
|[`string`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char.html#v:string)|`MonadParsec e s m => Tokens s -> m (Tokens s)`|Parses many characters matching the given string.|
|[`string'`](https://hackage.haskell.org/package/megaparsec-9.2.0/docs/Text-Megaparsec-Char.html#v:string-39-)|`(MonadParsec e s m, FoldCase (Tokens s)) => Tokens s -> m (Tokens s)`|Like `string`, but case-invariant.|
|[`makeExprParser`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Monad-Combinators-Expr.html#v:makeExprParser)|`MonadPlus m => m a -> [[Operator m a]] -> m a`|Allows parsing generic expressions, supporting infix, prefix, and postfix operators with precedences.|

### Further reading

<!--
* copy and paste links to blog/twitter etc from previous articles
-->

* [Parser Combinators in Elixir](https://serokell.io/blog/parser-combinators-in-elixir)
* [How to Implement an LR(1) Parser](https://serokell.io/blog/how-to-implement-lr1-parser)
* [Parsec: "try a <|> b" considered harmful](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/)
* [Quasiquotation 101](https://www.schoolofhaskell.com/user/marcin/quasiquotation-101)

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Dev](https://dev.to/serokell).

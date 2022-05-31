# Parsing with Haskell (Part 1): Lexing with Alex

<!--
* [X] generic introduction
-->

This is the first part of the two parts _Parsing with Haskell_ series.
Looking for the second part instead? You can find it [here](https://serokell.io/blog/parsing-with-happy).

A critical part of the majority of the compilers is grammar.
The grammar exposes the compiler's front-end as a convenient way to build programs.

This two-parts tutorial will look into two tools often used together by Haskellers to parse programs, namely Alex and Happy.

Both Alex and Happy are industrial-strength tools powerful enough to parse even Haskell itself.
At the bottom of this tutorial, you will find links to GHC's lexer and parser if you are curious to see how they look.

This tutorial was written using GHC version 9.0.2, Stack resolver LTS-19.8, Alex version 3.2.7.1, and Happy version 1.20.0.

## Our grammar: MiniML

<!--
* [X] describe what we support
* [X] example snippet
-->

For this tutorial, we will introduce a small grammar based on [ML](https://en.wikipedia.org/wiki/ML_(programming_language)), which we will call MiniML (read as "minimal").
As the name suggests, it should introduce a minimal syntax to get you started with Alex and Happy. Still, at the same time, we hope to introduce enough concepts so that you will be able to create a useful grammar for your programming language and use these tools effectively.

Some of the features that this tutorial series will support are:
* Variables, constants, function declarations, function applications, local bindings, and operators.
* Some simple types, as well as arrow types (functions).
* Conditional expressions: if-then-else and if-then.
* Block comments.
* The reader will be invited in the proposed exercises to extend the language with line comments and rational numbers. Later, in the second part, exercises will introduce patterns, list access, and pattern matching.

The snippet below demonstrates a program written in MiniML:

```sml
let the_answer : int =
  let a = 20 in
  let b = 1 in
  let c = 2 in
  a * c + b * c

let main (unit : ()) : () =
  print ("The answer is: " + the_answer)
```

## Lexing

Before we can start parsing, we should first write a lexer to the grammar, which is also known as _lexical analyzer_, _scanner_, and _tokenizer_.
According to A. W. Appel in _Modern Compiler Implementation in ML_ (p. 14):

> The lexical analyzer takes a stream of characters and produces a stream of names, keywords, and punctuation marks; it discards white space and comments between the tokens. It would unduly complicate the parser to have to account for possible white space and comments at every possible point; this is the main reason for separating lexical analysis from parsing.

We will use Alex as a tool to generate the lexical analyzer for our grammar.
It's the first step of the grammatical analysis for our programming language.
It will take the input stream of characters (a `String`, or in our case, a `ByteString`) representing the program written by the user and generate a stream of tokens (a list), which will be explained more in-depth shortly.

Note that we've mentioned that Alex will _generate_ a lexical analyzer and not that Alex is a lexical analyzer by itself.
Alex will read a `.x` file created by us specifying how to match lexemes and it will in turn create a `.hs` file which is the generated lexer.

In the second part of this tutorial, the token stream will be consumed by the parser generated Happy; a tool to generate syntactic analyzers.
Happy will try to match tokens according to specific rules that we will describe, where we will produce a tree structure representing a valid MiniML program.

Note that using Alex is not a requirement. You could also use, for example, parser combinators such as Megaparsec for lexing and Happy for parsing if you wanted to.

Making a lexer using parser combinators is pretty doable and manageable.
Meanwhile, using Alex is a bit more involved, but it has the advantage that performance will be more predictable; besides that, it can be easily integrated with Happy to output one token at a time, which may avoid creating a massive stream of tokens in the memory in the first place.

## Creating the project

We will use Stack to build the project.
Alex and Happy use files with extensions `.x` and `.y,` respectively, and Stack can automatically detect these file formats and build them.

n.b.: Using Stack is not strictly necessary. You may also use Cabal or plain Alex and Happy files, and it will work as well.

Begin by creating an empty Stack project:

```bash
$ stack new mini-ml simple
$ cd mini-ml
```

You will also need to have Alex and Happy installed on your machine:

```bash
$ stack install alex
$ stack install happy
```

We will use a `package.yaml` file for our project configuration.
Use your favorite text editor to initialize `package.yaml` with the following:

```yaml
name: mini-ml
version: 0.1.0.0

extra-source-files:
- README.md
- LICENSE

default-extensions:
- OverloadedStrings

library:
  source-dirs: src
  dependencies:
  - array
  - base
  - bytestring
  build-tools:
  - alex
  - happy
```

n.b.: You might need to use `hpack --force` to override the current Cabal file.

You may want to remove `src/Main.hs`, as we won't need it.

## Alex

<!--
* [X] link to its user guide
-->

Alex is a Haskell tool to generate lexers.

This section aims at giving an introduction to Alex and how you can use it to do useful things in conjunction with Happy (explained later).
Although Alex and Happy are frequently used together, they are independent tools. They may be combined with other technologies, so you may, for example, use Alex and Megaparsec together if you prefer.

### How it works

<!--
* [X] make a nifty flowchart
-->

It will be essential to differentiate between the terms **lexeme** and **token** during this section.
A lexeme is a valid atom of the grammar, such as a keyword (`in`, `if`, etc.), an operator (`+`, `/`, etc.), an integer literal, a string literal, a left or right parenthesis, an identifier, etc.
Meanwhile, a token consists of the _token name_ and an optional _token value_.
The token name is the name of a lexical category that the lexeme belongs to, while the token value is implemention-defined.
We can represent tokens as Haskell constructors with optionally the scanned lexeme. For instance, `In`, `If`, `Plus`, `Divide`, `Integer 42`, `String "\"foo\""`, `LPar`, `RPar`, and `Identifier "my_function"` are all tokens generated for each lexemes.

Lexers are often implemented using Deterministic Finite Automata (DFAs), which is a state machine. For example, supposing that our grammar has the keywords `if` and `in` and also identifiers consisting of only lowercase letters, a small automaton for it could look like so:

```text
                 ┌─┐
                 ▼ │ [a-z]
                ┌──┴┐
                │┌─┐│
        ┌──────►││4││◄──────┬───────┐
        │       │└─┘│       │       │
        │       └───┘       │       │
        │         ▲         │       │
        │ [a-z]   │ [a-z]   │       │
        │         │         │ [a-z] │
      ┌─┴─┐     ┌─┴─┐     ┌─┴─┐     │
      │   │ i   │┌─┐│ n   │┌─┐│     │
─────►│ 0 ├────►││1│├────►││2││     │
      │   │     │└─┘│     │└─┘│     │
      └───┘     └─┬─┘     └───┘     │
                  │                 │
                  │f                │
                  ▼                 │
                ┌───┐               │
                │┌─┐│ [a-z]         │
                ││3│├───────────────┘
                │└─┘│
                └───┘
```

To understand how to use the diagram above, imagine that this is a city where each numbered square is a place where you can drive to, and each arrow represents a one-way street.
The arrow coming from nowhere to the square with 0 is where you start driving, and squares with another square inside are places where you can park your car. In this case, you are not allowed to park it in the square with 0.

The arrows also have directions in them, guided by your GPS. Your GPS may tell you to drive straight into an `i`, then turn at an `[a-z]`, and then to stop, for example. However, your GPS may also ask you to take illegal turns, for example, to turn at an `@`, which will always lead you to a dead end, where you will be stuck.

To refine the terminology, the city is the _automaton_ (plural: _automata_) represented by a _state transition graph_, each numbered square is called a _state_, and each arrow is called a _transition_.
When a transition comes from nowhere and into a state, we say that this state is a _start state_, which is state 0 in this example.
The places were you can park are called _accept states_, i.e., 1, 2, 3, and 4.

The GPS represents the _input_ (string) of the automaton.
The dead ends represent an implicit _error state_ present in every DFA, which you can think of like a hidden state number 5, with all invalid transitions from every state to it.

To simplify matters, on ambiguous transitions such as `i` and `[a-z]`, assume that the one with a single letter takes precedence.

For example, to check the expression "int", we will follow this path: 0 → 1 → 2 → 4.

Now, we need to attribute meanings to each state so that this automaton can be useful, and we will do so like this:
* 0: Error.
* 1: Identifier.
* 2: In.
* 3: If.
* 4: Identifier.
* 5 (implicit): Error.

Since we stopped at 4, the result is that "int" is an identifier, so we get a token that is `Identifier "int"`.
The resulting token would be `In` if we stopped at 2 instead (for the expression "in").

What if we consumed no input and stopped at state 0, or the input was something like "Foo123" where we'd reach the implicit state 5?
In such cases, we halt and indicate a lexical error.

As started previously, we won't write our lexical analyzer from scratch. Instead, we will use Alex to generate a lexer that creates a stream of tokens, which the Happy-generated parser will parse.

### Regular expressions

Alex uses regular expressions (regexes) to match patterns, we'll keep it pretty simple and explain some of them which we'll use.

A syntax like `[0-9]` means that any digit character between `0` and `9` (inclusive) will be matched. Likewise, `[a-zA-Z]` means that any lowercase or uppercase character can be matched.
Characters may be excluded as well, for example, `[^\"]` reads as "anything except a double quote mark".

A `*` (called _Kleene star_) means that the expression can be matched zero or more times. Similarly, `+` means that it can be matched one or more times. For instance, `[0-9]+` can match `1234`. You may also find `?`, which means that an expression may match either zero or one time (i.e., it's optional).

A dot (`.`) means "match anything but a newline". You can also use common escape codes like `\n` which will match a newline.

An expression between strings, such as `">="`, means that it should only match exactly that string. An escaped character like `\?` can also match exactly that character.

You can group regexes with a space or a pipe (`|`) between them. A space means that Alex should match each regex successively, while the pipe means that it can alternate between choices, such as `0 (x [0-9a-fA-F]+ | o [0-7]+)`, which is a regex that can match hexadecimal or octal numbers.
Parentheses are used to group regexes together.

### Our first lexer

Begin by creating a new file: `src/Lexer.x`.
For now, let's add a "skeleton" for the file, which should look like the following:

```alex
{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  ) where
}

%wrapper "monadUserState-bytestring"

tokens :-

<0> $white+ ;

{
data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  = EOF
  deriving (Eq, Show)
}
```

We will use this small template to get Stack to compile the file.
Don't worry; I will explain everything there soon. :)

Running `stack build --fast --file-watch` will build the project, and Stack will automatically compile the Alex file into a Haskell file.

Alex files are divided into three sections.
* At the top of the file, we define the module and its imports, similarly to Haskell.
* In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
* At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.

The top and bottom sections are provided between the { and }, and all Haskell code will be inlined in the generated Stack file.

Curious to see the generated file? There are two ways in which you can access it.

1. If you run `alex src/Lexer.x`, it will generate `src/Lexer.hs`, where all generated Alex code will be, together with the provided snippets of your code.
2. Stack automatically generates a file whose path varies between OS and Cabal versions, but on my machine, it's in `.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.4.1.0/build/Lexer.hs`. You can also use `$(stack path --dist-dir)/build/Lexer.hs` to access this file.

For now, at the top of our file, we only declare the module name, but we will soon increase our export list and add more imports.

In the middle section, we first declare our _wrapper_, which indicates the type of code Alex should generate for us (`monadUserState`, which allows us to save custom state) and the input type (`bytestring`, but we could use plain Haskell `String`s instead).
Consult the Alex User Manual on [Wrappers](https://haskell-alex.readthedocs.io/en/latest/api.html#wrappers) if you want to see other wrappers.

The following section is `tokens :-`, where we will list all the patterns defined by regular expressions to match lexemes in our grammar plus an action on what the lexer should do with the matched lexeme.
The first definition we provided is `<0> $white+ ;`, which simply indicates that all white space should be skipped.
More on this later.

The bottom section contains some boilerplate that Alex requires us to write. These include a data type that needs to be called `AlexUserState`, a value with the initial state called `alexInitUserState`, and a value called `alexEOF`, which instructs Alex how to build the EOF (End-Of-File) token, reached when Alex has finished lexing the input string.

We add some additional datatypes: `Range`, `RangedToken`, and `Token`, which we will use throughout the article to describe the tokens that we've successfully created, as well as their positions. Saving the ranges is unnecessary, but it's an excellent addition when reporting errors.

Finally, for the EOF token, we use the `alexGetInput` action to retrieve the current position of the scanner and provide it to the token.

We should provide one token name for each lexical category in the grammar.
You are free to add new token names as you wish. Still, for this tutorial, we will have the following: operators, keywords, literals (strings and integers), identifiers, a token representing the end-of-file (EOF), etc.

Our token names will be as such:

```haskell
data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | String ByteString
  | Integer Integer
  -- Keywords
  | Let
  | In
  | If
  | Then
  | Else
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  -- Logical operators
  | And
  | Or
  -- Parenthesis
  | LPar
  | RPar
  -- Lists
  | Comma
  | LBrack
  | RBrack
  -- Types
  | Colon
  | Arrow
  -- EOF
  | EOF
  deriving (Eq, Show)
```

At the top of the file, make sure to include the appropriate imports:

```haskell
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
```

### Regex and regex macros

Alex uses regular expressions to match lexemes, so we will use them to recognize the various lexemes that should be available in our grammar.
Let's start with a simple one: identifiers.
We are free to choose the specification for identifiers, but let's use the following:

> An identifier is a sequence of alphanumeric characters, primes ('), question marks (?), and underscores (_).
> The identifier must begin with a letter or an underscore.

Alex allows the creation of _character set macros_, which are shortcuts that you can use to avoid duplicating macros. We use `$NAME = CHARACTER_SET` to define a character set macro.

```alex
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
```

We may also combine multiple regex macros into one, which will be appropriately expanded in its equivalent regular expression.
For example, here's how we can match an identifier:

```alex
$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-
```

We use `@NAME = REGEX` to define a regular expression macro. Note that macros may not be recursive.

#### Lexing identifiers

Let's define our first lexeme based on our identifier:

```alex
tokens :-

<0> $white+ ;

<0> @id     { tokId }
```
The syntax `<START_CODE> REGEX { CODE }` means that if Alex has managed to match a token `REGEX`, and it's currently in the `START_CODE` state, it should execute the code `CODE`.

Similarly, for white space, we can use `;` instead of giving it an explicit action, which simply means that Alex should do nothing interesting with it. This is the same as writing `{ skip }`.

The idea of start codes is a bit more sophisticated yet valuable.
Alex works as a state machine, and `0` indicates the initial code in which the machine starts.
In this case, we can match white space and identifiers when we start in state `0`.
We will later create other start codes.

`CODE` may contain any Haskell expression, as it will be included verbatim in the generated code.
Alex expects that this expression will have type `AlexAction RangedToken`. Here's the automatically-genereated definition for `AlexAction` when using `monadUserState-bytestring`:

```haskell
type AlexAction result = AlexInput -> Int64 -> Alex result
```

Where `result` will be `RangedToken` in our case.

Let's create our `tokId` function. We do so by inserting this definition at the bottom of the file:

```haskell
mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }
```

`tokId` will extract the lexeme from the input string, which will be the first `len` characters of `str`.
The range will then be handled by `mkRange`, a function that advances the position accordingly to each seen character in the input string using `alexMove`. Alex automatically generates this function, and you don't need to include it with your code.

We should check whether our code works. I will define a small function that we can use for testing. Don't forget to export it!

```haskell
scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
```

Startup GHCi and let's check whether this works. It should result in something like this (slightly prettified for ease of visualization):

```haskell
>>> runAlex "my_identifier" alexMonadScan
Right (RangedToken {rtToken = Identifier "my_identifier", rtRange = Range {start = AlexPn 0 1 1, stop = AlexPn 13 1 14}})
>>> scanMany "my_identifier other'identifier ALL_CAPS"
Right
  [ RangedToken {rtToken = Identifier "my_identifier", rtRange = Range {start = AlexPn 0 1 1, stop = AlexPn 13 1 14}}
  , RangedToken {rtToken = Identifier "other'identifier", rtRange = Range {start = AlexPn 14 1 15, stop = AlexPn 30 1 31}}
  , RangedToken {rtToken = Identifier "ALL_CAPS", rtRange = Range {start = AlexPn 31 1 32, stop = AlexPn 39 1 40}}
  , RangedToken {rtToken = EOF, rtRange = Range {start = AlexPn 39 1 40, stop = AlexPn 39 1 40}}
  ]
```

The `AlexPn` values mean the following, respectively: the number of characters before the token, the line number, and the line column.

Pretty cool stuff.

If you are stuck or want to check that we're on the same page, the code for the lexer up to this section may be found [here](https://gist.github.com/heitor-lassarote/c8f12feab735439a50015b7c60d4f21a).

#### Lexing keywords and operators

We can now lex identifiers, but a programming language typically consists of much more than only this.
Let's now scan other keywords that we defined.
Thankfully, they are pretty simple:

```alex
-- Keywords
<0> let     { tok Let }
<0> in      { tok In }
<0> if      { tok If }
<0> then    { tok Then }
<0> else    { tok Else }

-- Arithmetic operators
<0> "+"     { tok Plus }
<0> "-"     { tok Minus }
<0> "*"     { tok Times }
<0> "/"     { tok Divide }

-- Comparison operators
<0> "="     { tok Eq }
<0> "<>"    { tok Neq }
<0> "<"     { tok Lt }
<0> "<="    { tok Le }
<0> ">"     { tok Gt }
<0> ">="    { tok Ge }

-- Logical operators
<0> "&"     { tok And }
<0> "|"     { tok Or }

-- Parenthesis
<0> "("     { tok LPar }
<0> ")"     { tok RPar }

-- Lists
<0> "["     { tok LBrack }
<0> "]"     { tok RBrack }
<0> ","     { tok Comma }

-- Types
<0> ":"     { tok Colon }
<0> "->"    { tok Arrow }

-- Identifiers
<0> @id     { tokId }
```

We also need to define `tok`:

```haskell
tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }
```

There should be no mystery to this function. It simply inserts the provided `Token` and creates a range for it.

We can now check it out in GHCi:

```haskell
>>> scanMany "if true then foo else (bar baz)"
Right
  [ RangedToken {rtToken = If, rtRange = Range {start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
  , RangedToken {rtToken = Identifier "true", rtRange = Range {start = AlexPn 3 1 4, stop = AlexPn 7 1 8}}
  , RangedToken {rtToken = Then, rtRange = Range {start = AlexPn 8 1 9, stop = AlexPn 12 1 13}}
  , RangedToken {rtToken = Identifier "foo", rtRange = Range {start = AlexPn 13 1 14, stop = AlexPn 16 1 17}}
  , RangedToken {rtToken = Else, rtRange = Range {start = AlexPn 17 1 18, stop = AlexPn 21 1 22}}
  , RangedToken {rtToken = LPar, rtRange = Range {start = AlexPn 22 1 23, stop = AlexPn 23 1 24}}
  , RangedToken {rtToken = Identifier "bar", rtRange = Range {start = AlexPn 23 1 24, stop = AlexPn 26 1 27}}
  , RangedToken {rtToken = Identifier "baz", rtRange = Range {start = AlexPn 27 1 28, stop = AlexPn 30 1 31}}
  , RangedToken {rtToken = RPar, rtRange = Range {start = AlexPn 30 1 31, stop = AlexPn 31 1 32}}
  , RangedToken {rtToken = EOF, rtRange = Range {start = AlexPn 31 1 32, stop = AlexPn 31 1 32}}
  ]
```

### Lexing integers

Lexing integers is pretty straightforward. We can just use a sequence of one or more digits.

```alex
-- Constants
<0> $digit+ { tokInteger }
```

And of course, we need to define `tokInteger`:

```haskell
tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }
```

Check that it works:

```haskell
>>> scanMany "42"
Right
  [ RangedToken {rtToken = Integer 42, rtRange = Range {start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
  , RangedToken {rtToken = EOF, rtRange = Range {start = AlexPn 2 1 3, stop = AlexPn 2 1 3}}
  ]
```

#### Lexing comments

Lexing comments will require us to use an extra start code besides just `0`.
Surely, we could make something as simple as the following:

```alex
<0> "(*" (. | \n)* "*)" ;
```

But this will lead to some problems.
For instance, what if the comment is unclosed?

```haskell
>>> scanMany "(* my comment"
Right
  [ RangedToken {rtToken = LPar, rtRange = Range {start = AlexPn 0 1 1, stop = AlexPn 1 1 2}}
  , RangedToken {rtToken = Times, rtRange = Range {start = AlexPn 1 1 2, stop = AlexPn 2 1 3}}
  , RangedToken {rtToken = Identifier "my", rtRange = Range {start = AlexPn 3 1 4, stop = AlexPn 5 1 6}}
  , RangedToken {rtToken = Identifier "comment", rtRange = Range {start = AlexPn 6 1 7, stop = AlexPn 13 1 14}}
  , RangedToken {rtToken = EOF, rtRange = Range {start = AlexPn 13 1 14, stop = AlexPn 13 1 14}}
  ]
```

We would like to detect such cases and also support nested comments.
We will create two helper functions, `nestComment` and `unnestComment`, which will keep track of how many layers of comments we have and allow us to detect whether we reached the end of the file with an unclosed comment.

In addition, Alex provides the `andBegin` combinator that will execute an expression and change the current start code.

```alex
<0>       "(*" { nestComment `andBegin` comment }
<0>       "*)" { \_ _ -> alexError "Error: unexpected closing comment" }
<comment> "(*" { nestComment }
<comment> "*)" { unnestComment }
<comment> .    ;
<comment> \n   ;
```

The critical thing to notice is that we can start a comment anywhere, as it has start code `0`, but we can only match a closing comment pair if we begin to match a comment in the first place, thanks to the `comment` start code.

Note that checking for the `*)` lexeme at the start code `0` is unnecessary.
Without it, it would be emitted as a multiplication followed by a closing parenthesis, but the parser would catch it later.
Like Haskell, MiniML will support passing operators as first-class functions, like `let add = (+)`.
However, this has the same deficiency as OCaml: `(*)` is recognized as a comment.
However, `( *)` is accepted by OCaml but emits a message saying, "Warning : this is not the end of a comment.".
Because of this, we will emit an error using `alexError` to the user and expect that `( * )` will be used to reference the multiplication operator instead.
Alternatively, you can change comments' syntax to something else here, such as `/* */`, and avoid this ambiguity, but that wouldn't feel too ML-like.

Next, we will change `AlexUserState` so it remembers the nesting level of our comments. We will initialize it with 0.
We also define some helper functions for dealing with our state.

```haskell
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())
```

n.b.: If you're familiar with the [`mtl`](https://hackage.haskell.org/package/mtl) library, you might prefer to define `instance MonadState Alex`.

We simply increase the nesting value and skip the consumed input to nest a comment.
Note that `skip input len` is simply `alexMonadScan`, which asks to scan the next token.
Unnesting is similar, except that we decrease the level, and if we reach level 0, we must return to our initial start code (`0`) as it means we exited the comment.
Don't forget to import `when` from `Control.Monad`.

```haskell
nestComment, unnestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len
```

Finally, let's change `alexEOF` to emit an error if we end up with an unclosed comment by checking whether we were parsing a comment when we reached EOF.

```haskell
alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)
```

Trying it out in GHCi yields the expected results.

```haskell
>>> scanMany "(* my comment"
Left "Error: unclosed comment"
>>> scanMany "if (* my (* nested *) \n comment *) x"
Right
  [ RangedToken {rtToken = If, rtRange = Range {start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
  , RangedToken {rtToken = Identifier "x", rtRange = Range {start = AlexPn 35 2 13, stop = AlexPn 36 2 14}}
  , RangedToken {rtToken = EOF, rtRange = Range {start = AlexPn 36 2 14, stop = AlexPn 36 2 14}}
  ]
```

#### Lexing strings

Lexing strings should have no surprises. Just stick this in your lexer:

```alex
<0> \"[^\"]*\" { tokString }
```

And we use this definition for `tokString`:

```haskell
tokString :: AlexAction RangedToken
tokString inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = String $ BS.take len str
    , rtRange = mkRange inp len
    }
```

This is a pretty basic string lexer. It does not accept escaped characters or nested quote marks.
We invite the reader to extend the string lexer to be more useful in the exercise below.

You can find the complete code for the lexer until now [here](https://gist.github.com/heitor-lassarote/e56e5330368f2cf9265a6eefa8a7a880).

#### Exercises

1. We support block comments with `(* *)`. Change the scanner to accept line comments, such as `# foo`, which end when they find a newline or EOF.

2. Change the string lexer to accept the following escaped characters: `\\`, `\n`, and `\t`.
Feel free to add any other escape codes that you can think about.

**Hint**: Create a buffer in `AlexUserState` that adds characters as they are seen and two auxiliary `enterString` and `exitString` functions.
Entering the string, save the current position in the state, add `\"` to the buffer, and begin a new start code.
For a escape character matched with `\\n`, `\\t` or `\\\\`, add `\n`, `\t`, or `\\` to the buffer with a new `emit` function.
For an ordinary character matched with a `.`, add it to the current buffer with a new `emitCurrent` function.
Don't forget to match `\\\"` as an escaped quote mark and add it with `emit`.
Emit the string as a new token when you exit the string state, reseting the string state, and don't forget to add the closing quote mark. Make sure that the end position is also advanced by one character.
Don't forget to check if there is an unclosed string like we did for the block comments.

For inspiration on doing this, you can use Alex's [Tiger example](https://github.com/haskell/alex/blob/master/examples/tiger.x) as a guide.

You can find the solutions to both exercises [here](https://gist.github.com/heitor-lassarote/a6ec93ae4cf5ebc218c335017140342b).

## Happy

In this tutorial, we demonstrated how to build a lexer using Alex to turn lexemes into tokens, using start codes and a user state adequate for MiniML.

Your next step in this journey is now to use the tokens generated by Alex to create a parser.
Continue reading the [part 2](https://serokell.io/blog/parsing-with-happy) of this series where you will be introduced to Happy to create the parser of the grammar.

## Further reading

<!--
* [X] copy and paste links to blog/Twitter etc. from previous articles
-->

If you liked this article, you might also enjoy these resources:

* [Alex User Guide](https://haskell-alex.readthedocs.io/en/latest/).
* GHC's [Alex](https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser/Lexer.x) and [Happy](https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y) files.
* APPEL, A. W. (1998). _Modern Compiler Implementation in ML_, Cambridge University Press.

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Dev](https://dev.to/serokell).

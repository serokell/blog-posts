# Alex and Happy

<!--
* [X] generic introduction
-->

A critical part of the majority of the compilers is grammar.
The grammar exposes the compiler's front-end as a convenient way to build programs.

This tutorial will look into two tools often used together by Haskellers to parse programs, namely Alex and Happy.

Both Alex and Happy are industrial-strength tools powerful enough to parse even Haskell itself.
At the bottom of this tutorial, you will find links to GHC's lexer and parser if you are curious to see how they look.

This tutorial was written using GHC version 9.0.2, Stack resolver LTS-19.8, Alex version 3.2.7.1, and Happy version 1.20.0.

## Comparison with parser combinators

<!--
* [X] refer to parser combinators article
* [X] talk about the pros and cons
-->

In a previous tutorial, we discussed implementing and using [Parser Combinators in Haskell](https://serokell.io/blog/parser-combinators-in-haskell).
In the article, we discussed some pros and cons of LL parsers compared to LR parsers, which we will reiterate and elaborate on.

<!--
TODO: not sure how correct is the theory here, a more in-depth review here would be nice
-->

### Lexing

One step we've skipped in the previous article is **lexing**. According to A. W. Appel in _Modern Compiler Implementation in ML_ (p. 14):

> The lexical analyzer takes a stream of characters and produces a stream of names, keywords, and punctuation marks; it discards white space and comments between the tokens. It would unduly complicate the parser to have to account for possible white space and comments at every possible point; this is the main reason for separating lexical analysis from parsing.

Making a lexer using parser combinators is pretty doable and manageable.
Meanwhile, using Alex is a bit more involved, but it has the advantage that performance will be more predictable; besides that, it can be easily integrated with Happy to output one token at a time, which may avoid creating a massive list of tokens in the memory in the first place.

Note that using Alex is not a requirement. You could also use, for example, Megaparsec for lexing and Happy for parsing if you wanted to.

### Parsing

Haskell libraries such as Megaparsec are typically exposed as a LL(1) or LL(∞) parser.
LL here means **L**eft-to-right parsing, **L**eftmost derivation, as they scan strings from left to right and build a tree deriving them from the left (see below).

The number between the parentheses indicates how many look-ahead tokens the parser can see.
A parser with a look-ahead of `k` can make decisions by looking `k` tokens (or characters) ahead.
A look-ahead of `k=1` is often better for better performance, and it's enough to parse most grammars in programming languages.

On the other hand, Happy is an LR(1)\* parser, where the R means **R**ightmost derivation in reverse.

\* Happy is actually an LALR(1) (Look-Ahead LR) parser, which is a more performant albeit less powerful variant of an LR(1) parser; see [section 8.4.1](https://monlih.github.io/happy-docs/#_sec_conflict_tips) of the Happy User Guide.
Happy also supports GLR (Generalized LR) parsing, which this tutorial won't cover, but you can find more about it in the [section 3](https://monlih.github.io/happy-docs/#_sec_glr) of the Happy User Guide.

As an example, suppose we have the expression `1 + 2 + 3`, which is parsed as the following tree:

```text
                +
              ↙   ↘
            +       3
          ↙   ↘
        1       2
```

It means that in an expression such as `1 + 2 + 3`, a LL parser would expand the tree visiting the leftmost nodes first, visiting the tokens in this order:

<pre>
<u>exp</u>
<b><u>exp</u> + exp</b>
<b><u>exp</u> + exp</b> + exp
<b>1</b> + <u>exp</u> + exp
1 + <b>2</b> + <u>exp</u>
1 + 2 + <b>3</b>
</pre>

n.b.: **Bold** means the result of the derivation, and <u>underline</u> means the derivation that will be expanded next.

Whilst an LR parser would do the following:

<pre>
<u>exp</u>
<b>exp + <u>exp</u></b>
exp + <b>exp + <u>exp</u></b>
exp + <u>exp</u> + <b>3</b>
<u>exp</u> + <b>2</b> + 3
<b>1</b> + 2 + 3
</pre>

For more information on leftmost and rightmost derivations, see [LL and LR Parsing Demystified](https://blog.reverberate.org/2013/07/ll-and-lr-parsing-demystified.html).

Another difference is that Alex is a lexer generator and Happy a parser generator.
It means that instead of writing Haskell files (`.hs`) like in parser combinators, you will write Alex (`.x`) and Happy (`.y`) files, which in turn will automatically generate Haskell files with the described grammar.

Below are some pros and cons of Happy in comparison to, for example, Megaparsec:

Pros:
* LR parsers are more powerful than LL, allowing to parse more complex grammars with greater ease.
* Ambiguous grammars are reported to the user, allowing a greater trust in the parser's output and saving the debugging headache.
* LR grammars can handle left-recursion, while LL parsers can't.

Cons:
* Less idiomatic, as you need to write Alex and Happy grammars instead of Haskell code.
* Less flexible since you can't touch the algorithm on how Happy works. Meanwhile, you can easily create your functions for parser combinators.

Comparing the implementations of both parsing techniques is beyond the scope of this post. Still, if you are interested, you may read the parser combinators article listed above, as well as [How to Implement an LR(1) Parser](https://serokell.io/blog/how-to-implement-lr1-parser).

## Our grammar: MiniML

<!--
* [X] describe what we support
* [X] example snippet
-->

For this tutorial, we will introduce a small grammar based on [ML](https://en.wikipedia.org/wiki/ML_(programming_language)), which we will call MiniML (read as "minimal").
As the name suggests, it should introduce a minimal syntax to get you started with Alex and Happy. Still, at the same time, we hope to introduce enough concepts so that you will be able to create a useful grammar for your programming language and use these tools effectively.

Some of the features that this tutorial will support are:
* Variables, constants, function declarations, function applications, local bindings, and operators.
* Some simple types, as well as arrow types (functions).
* Conditional expressions: if-then-else and if-then.
* Block comments.
* The reader will be invited in the proposed exercises to extend the language with line comments, tuples, type declarations, and pattern matching.

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

Alex is a Haskell tool to generate lexical analyzers (lexers).

This section aims at giving an introduction to Alex and how you can use it to do useful things in conjunction with Happy (explained later).
Although Alex and Happy are frequently used together, they are independent tools. They may be combined with other technologies, so you may, for example, use Alex and Megaparsec together if you prefer.

You may also want to read the [Alex User Guide](https://haskell-alex.readthedocs.io/en/latest/).

### How it works

<!--
* [X] make a nifty flowchart
-->

It will be essential to differentiate between the terms **lexeme** and **token** during this section.
A lexeme is a valid atom of the grammar, such as a keyword (`in`, `if`, etc.), an operator (`+`, `/`, etc.), an integer literal, a string literal, a left or right parenthesis, an identifier, etc.
Meanwhile, a token is a class that a specific lexeme belongs to, which will be represented as a Haskell constructor. For instance, `In`, `If`, `Plus`, `Divide`, `Integer 42`, `String "\"foo\""`, `LPar`, `RPar`, and `Identifier "my_function"` are all tokens generated from lexemes.

Lexers are often implemented using Deterministic Finite Automata (FDAs), which you can think of as a state machine. For example, supposing that our grammar has the keywords `if` and `in` and also identifiers consisting of only lowercase letters, a small automaton for it could look like so:

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

To simplify matters, on ambiguous transitions such as `i` and `[a-z]`, assume that the one with a single letter takes precedence.

State 0 is the initial state where the automaton begins. Suppose that the automaton can stop at states 1, 2, 3, and 4.

If we want to check the expression "int", we will follow this path: 0 → 1 → 2 → 4.

Now, we need to attribute meanings to each state so that this automaton can be useful, and we will do so like this:
* 0: Error.
* 1: Identifier.
* 2: In.
* 3: If.
* 4: Identifier.

Since we stopped at 4, the result is that "int" is an identifier, so we get a token that is `Identifier "int"`.
The resulting token would be 'In' if we stopped at 2 instead (for the expression "in").


What if we consumed no input and stopped at state 0, or the input was something like "Foo123" where there aren't valid transitions?
In such cases, we halt and indicate a lexical error.

We won't write our lexical analyzer from scratch. Instead, we will use Alex to create a list of tokens, which Happy will parse.

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
  , Token (..)
  , TokenType (..)
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

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ Token EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data Token = Token
  { tType :: TokenType
  , tRange :: Range
  } deriving (Eq, Show)

data TokenType
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

The following section is `tokens :-`, where we will list all the lexemes in our grammar and an optional action on what the lexer should do with the matched lexeme.
The first definition we provided is `<0> $white+ ;`, which simply indicates that all whitespace should be ignored.
More on this later.

The bottom section contains some boilerplate that Alex requires us to write. These include a data type that needs to be called `AlexUserState`, a value with the initial state called `alexInitUserState`, and a value called `alexEOF`, which instructs Alex how to build the EOF (End-Of-File) token, reached when Alex has finished lexing the input string.

We add some additional datatypes: `Range`, `Token`, and `TokenType`, which we will use throughout the article to describe the tokens that we've successfully created, as well as their positions. Saving the ranges is unnecessary, but it's an excellent addition when reporting errors.

Finally, for the EOF token, we use the `alexGetInput` action to retrieve the current position of the scanner and provide it to the token.

We should provide one token type for each lexeme in the grammar.
You are free to choose the available tokens. Still, for this tutorial, we will have the following: operators, keywords, literals (strings and integers), identifiers, a token representing the end-of-file (EOF), etc.

Our token types will be as such:

```haskell
data TokenType
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

@id = ($alpha | \_) ($alpha | $digit | \_ | ' | \?)*

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

Similarly, for whitespace, we can use `;` instead of giving it an action, which simply means that Alex should do nothing with it.

The idea of start codes is a bit more sophisticated yet valuable.
Alex works as a state machine, and `0` indicates the initial code in which the machine starts.
In this case, we can match whitespaces and identifiers while we are in state `0`.
We will later create other start codes for matching specific things where we wouldn't like to match anything else arbitrarily.

`CODE` may contain any Haskell expression, as it will be included verbatim in the generated code.
Alex expects that this expression will have type `AlexAction Token`. Here's the automatically-genereated definition for `AlexAction` when using `monadUserState-bytestring`:

```haskell
type AlexAction result = AlexInput -> Int64 -> Alex result
```

Where `result` will be `Token` in our case.

Let's create our `tokId` function. We do so by inserting this definition at the bottom of the file:

```haskell
mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction Token
tokId inp@(_, _, str, _) len =
  pure Token
    { tType = Identifier $ BS.take len str
    , tRange = mkRange inp len
    }
```

`tokId` will extract the lexeme from the input string, which will be the first `len` characters of `str`.
The range will then be handled by `mkRange`, a function that advances the position accordingly to each seen character in the input string using `alexMove`. Alex automatically generates this function, and you don't need to include it with your code.

We should check whether our code works. I will define a small function that we can use for testing. Don't forget to export it!

```haskell
scanMany :: ByteString -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if tType output == EOF
        then pure [output]
        else (output :) <$> go
```

Startup GHCi and let's check whether this works. It should result in something like this (slightly prettified for ease of visualization):

```haskell
>>> runAlex "my_identifier" alexMonadScan
Right (Token {tType = Identifier "my_identifier", tRange = Range {start = AlexPn 0 1 1, stop = AlexPn 13 1 14}})
>>> scanMany "my_identifier other'identifier ALL_CAPS"
Right
  [ Token {tType = Identifier "my_identifier", tRange = Range {start = AlexPn 0 1 1, stop = AlexPn 13 1 14}}
  , Token {tType = Identifier "other'identifier", tRange = Range {start = AlexPn 14 1 15, stop = AlexPn 30 1 31}}
  , Token {tType = Identifier "ALL_CAPS", tRange = Range {start = AlexPn 31 1 32, stop = AlexPn 39 1 40}}
  , Token {tType = EOF, tRange = Range {start = AlexPn 39 1 40, stop = AlexPn 39 1 40}}
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
tok :: TokenType -> AlexAction Token
tok ctor inp len =
  pure Token
    { tType = ctor
    , tRange = mkRange inp len
    }
```

There should be no mystery to this function. It simply inserts the provided `TokenType` and creates a range for it.

We can now check it out in GHCi:

```haskell
>>> scanMany "if true then foo else (bar baz)"
Right
  [ Token {tType = If, tRange = Range {start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
  , Token {tType = Identifier "true", tRange = Range {start = AlexPn 3 1 4, stop = AlexPn 7 1 8}}
  , Token {tType = Then, tRange = Range {start = AlexPn 8 1 9, stop = AlexPn 12 1 13}}
  , Token {tType = Identifier "foo", tRange = Range {start = AlexPn 13 1 14, stop = AlexPn 16 1 17}}
  , Token {tType = Else, tRange = Range {start = AlexPn 17 1 18, stop = AlexPn 21 1 22}}
  , Token {tType = LPar, tRange = Range {start = AlexPn 22 1 23, stop = AlexPn 23 1 24}}
  , Token {tType = Identifier "bar", tRange = Range {start = AlexPn 23 1 24, stop = AlexPn 26 1 27}}
  , Token {tType = Identifier "baz", tRange = Range {start = AlexPn 27 1 28, stop = AlexPn 30 1 31}}
  , Token {tType = RPar, tRange = Range {start = AlexPn 30 1 31, stop = AlexPn 31 1 32}}
  , Token {tType = EOF, tRange = Range {start = AlexPn 31 1 32, stop = AlexPn 31 1 32}}
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
tokInteger :: AlexAction Token
tokInteger inp@(_, _, str, _) len =
  pure Token
    { tType = Integer $ read $ BS.unpack $ BS.take len str
    , tRange = mkRange inp len
    }
```

Check that it works:

```haskell
>>> scanMany "42"
Right
  [ Token {tType = Integer 42, tRange = Range {start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
  , Token {tType = EOF, tRange = Range {start = AlexPn 2 1 3, stop = AlexPn 2 1 3}}
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
  [ Token {tType = LPar, tRange = Range {start = AlexPn 0 1 1, stop = AlexPn 1 1 2}}
  , Token {tType = Times, tRange = Range {start = AlexPn 1 1 2, stop = AlexPn 2 1 3}}
  , Token {tType = Identifier "my", tRange = Range {start = AlexPn 3 1 4, stop = AlexPn 5 1 6}}
  , Token {tType = Identifier "comment", tRange = Range {start = AlexPn 6 1 7, stop = AlexPn 13 1 14}}
  , Token {tType = EOF, tRange = Range {start = AlexPn 13 1 14, stop = AlexPn 13 1 14}}
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

set :: AlexUserState -> Alex ()
set s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())
```

We simply increase the nesting value and skip the consumed input to nest a comment.
Note that `skip input len` is simply `alexMonadScan`, which asks to scan the next token.
Unnesting is similar, except that we decrease the level, and if we reach level 0, we must return to our initial start code (`0`) as it means we exited the comment.
Don't forget to import `when` from `Control.Monad`.

```haskell
nestComment, unnestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  set state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len
```

Finally, let's change `alexEOF` to emit an error if we end up with an unclosed comment by checking whether we were parsing a comment when we reached EOF.

```haskell
alexEOF :: Alex Token
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ Token EOF (Range pos pos)
```

Trying it out in GHCi yields the expected results.

```haskell
>>> scanMany "(* my comment"
Left "Error: unclosed comment"
>>> scanMany "if (* my (* nested *) \n comment *) x"
Right
  [ Token {tType = If, tRange = Range {start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
  , Token {tType = Identifier "x", tRange = Range {start = AlexPn 35 2 13, stop = AlexPn 36 2 14}}
  , Token {tType = EOF, tRange = Range {start = AlexPn 36 2 14, stop = AlexPn 36 2 14}}
  ]
```

#### Lexing strings

Lexing strings should have no surprises. Just stick this in your lexer:

```alex
<0> \".*\"  { tokString }
```

And we use this definition for `tokString`:

```haskell
tokString :: AlexAction Token
tokString inp@(_, _, str, _) len =
  pure Token
    { tType = String $ BS.take len str
    , tRange = mkRange inp len
    }
```

This is a pretty basic string lexer. It does not accept escaped characters or nested quote marks.
We invite the reader to extend the string lexer to be more useful in the exercise below.

You can find the complete code for the lexer until now [here](https://gist.github.com/heitor-lassarote/e56e5330368f2cf9265a6eefa8a7a880).

#### Exercises

1. Create a scanner for fractional numbers.
Besides accepting numbers such as `3.14`, it should also accept exponents, like `12.5e-4`.

2. We support block comments with `(* *)`. Change the scanner to accept line comments, such as `// foo`, which end when they find a newline or EOF.

3. Change the string lexer to accept the following escaped characters: `\\`, `\n`, and `\t`.
Feel free to add any other escape codes that you can think about.

**Hint**: Create a buffer in `AlexUserState` that adds characters as they are seen and two auxiliary `enterString` and `exitString` functions.
Entering the string, save the current position in the state, and begin a new start code.
For an ordinary character matched with a `.`, just add it to the current string.
For a escape character matched with `\\n`, `\\t` or `\\`, add `\n`, `\t`, or `\` to the buffer.
Don't forget to match `\\\"` as an escaped quote mark.
Emit the string as a new token when you exit the string state.
Don't forget to check if there is an unclosed string like we did for the block comments.

For inspiration on doing this, you can use Alex's [Tiger example](https://github.com/haskell/alex/blob/master/examples/tiger.x) as a guide.

## Happy

<!--
* [X] refer to LR(1) article
* [X] link to its user guide
-->

As with Alex, our goal here is to provide enough information on Happy enough to be productive and do valuable things with it so that we won't visit every single feature of Happy.
Please refer to the [Happy User Guide](https://monlih.github.io/happy-docs/).

### How it works

An in-depth explanation of how an LR(1) parser works is well beyond the scope of this tutorial, but for this, we recommend you read our article, [How to Implement an LR(1) Parser](https://serokell.io/blog/how-to-implement-lr1-parser).
We will, however, introduce a few basic concepts required to understand this article better.

#### Productions, terminals and non-terminals

A **production rule** is a parsing rule, which we will use to describe our grammar.
It looks like so in Happy:

```happy
myProduction
  : productionA1 productionA2 ... { actionA }
  | productionB1 productionB2 ... { actionB }
  | ...
```

The ellipses here are not correct Happy grammar, and we use them to represent that more productions may be present.

`production@#` where `@` is some letter and `#` some number means that the parser must parse each production in succession.
The pipe (`|`) represents an alternation. The parser may parse any of the given productions, accepting the one that matches.
An alternative may be empty, meaning that `myProduction : { action }` is also accepted.
`action@` represents what the parser should do once successfully parsed that rule. Like in Alex, it may be any Haskell expression. More about this later.

Production rules may also have an optional type signature.
For instance, the declaration of `myProduction` could also have been written as such:

```happy
myProduction :: { MyType }
```

Where any Haskell type may replace `MyType`.

Later in the article, we will also use the following notation representing the same as above.
This is not valid Happy code, but it's what it uses for debugging information.

```
myProduction -> productionA1 productionA2 ...
myProduction -> productionB1 productionB2 ...
myProduction -> ...
```

There may be multiple productions in a single Happy file.

A **terminal** is a production that has no children.
In our grammar, the tokens produced by Alex will be terminals.

A **non-terminal** is a production that consists of a group of various other productions (they may be terminal or non-terminal).
In the example above, `myProduction` is non-terminal.

#### Parser position

It's essential to introduce a notation that will be used later in this article.
We use a dot (`.`) to represent the current position of the parser.
For example, suppose we have the following:

```
exp -> exp . '+' exp
```

In this case, the parser has finished consuming the first `exp`, and it's about to consume the `+`.

#### Look-ahead

The **look-ahead** represents the sequence of symbols that will be consumed next by the parser.
Happy is an LALR(1) parser, which means its look-ahead is 1, so it can see one incoming symbol.
In the `exp` example above, the look-ahead token will be `+`.

#### Actions (shift, reduce, goto, accept)

When working with Happy, two core concepts that you need to know are **shifting** and **reducing**, which I'll briefly explain.

An LR parser is also implemented as a state machine, like Alex. The parser contains a stack with all the tokens consumed during parsing.
The parser produces an action table describing how to make decisions based on its stack and the look-ahead token.

**Shifting** means that the parser should push the look-ahead token on its internal stack, change (shift) its state, and continue parsing.

**Reducing** means that the parser has accepted a production, and so it will pop tokens from the stack, run an action with the consumed input, and return to a previous state.

Later on, we will introduce the concept of shift/reduce and reduce/reduce conflicts, which are ways that Happy tells us that the grammar is ambiguous.

There are also **goto** and **accept** actions.
Goto simply means that the parser should change states without consuming input.
Accept is the last action performed by the parser when it has seen all input and reached EOF—accepting means that the program was successfully parsed.

But enough with theory, let's go to the practice and learn more concepts as we go through it.

### Our first parser

Below is a minimal definition of a grammar file that Happy can compile.
In addition, we inserted an extra `empty` rule which we will delete later, as it's there only to make Happy happy.

Make sure to put it in a new `Parser.y` file.
Like in Alex, the name doesn't need to be `Parser`, but it's essential to keep the `.y` extension.

```happy
{
{-# LANGUAGE DeriveFoldable #-}
module Parser
  ( parseMiniML
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
}

%name parseMiniML
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.Token L.EOF _ }

%%

empty : {}  -- only to get the file compiling; we will remove this

{
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}
```

Happy has a similar structure to Alex, where we insert Haskell definitions at the top and the bottom of the file, with our parser being in the middle.
Likewise, anything between `{` and `}` is going to be inlined as well.

Let's quickly visit each part of this file, from top to bottom.
* The header contains the code that will generate the header of our Haskell file. Happy will generate the primary parsing function, `parseMiniML`, which we export.
  * `DeriveFoldable` allows us to write `deriving (Foldable)`, which will be helpful later.
  * We also import some extra things that will be useful in utilities later.
* The following lines instruct Happy with extra information to generate the parser. The things we should provide to it include:
  * One or more names for our parser. The syntax should be `%name PARSER_NAME [PRODUCTION]`. If the `PRODUCTION` is not given, it uses the first non-terminal that appears in the file.
  * What is the type of our tokens with `%tokentype`.
  * Which function should it call if there is a parse error with `%error`.
  * What is the monad that should be used and its canonical operations with `%monad` (we reuse `Alex` for simplicity).
  * Which function lexes tokens and how to extract the EOF token with `%lexer`.
* `%%`, after which we must specify the production rules for the grammar.
* A trailer to include more Haskell definitions, such as auxiliary files and the AST that will be built by the parser. You can also define those in another module if you prefer.

#### Tokens

Since Happy operates on tokens, we must instruct it on how to match each token.
This is done using `%token`, which must be placed before the `%%` in the file.
We list each token name that we've defined in our lexer and a Haskell pattern between braces indicating how to match it.

```alex
%token
  -- Identifiers
  identifier { L.Token (L.Identifier _) _ }
  -- Constants
  string     { L.Token (L.String _) _ }
  integer    { L.Token (L.Integer _) _ }
  -- Keywords
  let        { L.Token L.Let _ }
  in         { L.Token L.In _ }
  if         { L.Token L.If _ }
  then       { L.Token L.Then _ }
  else       { L.Token L.Else _ }
  -- Arithmetic operators
  '+'        { L.Token L.Plus _ }
  '-'        { L.Token L.Minus _ }
  '*'        { L.Token L.Times _ }
  '/'        { L.Token L.Divide _ }
  -- Comparison operators
  '='        { L.Token L.Eq _ }
  '<>'       { L.Token L.Neq _ }
  '<'        { L.Token L.Lt _ }
  '<='       { L.Token L.Le _ }
  '>'        { L.Token L.Gt _ }
  '>='       { L.Token L.Ge _ }
  -- Logical operators
  '&'        { L.Token L.And _ }
  '|'        { L.Token L.Or _ }
  -- Parenthesis
  '('        { L.Token L.LPar _ }
  ')'        { L.Token L.RPar _ }
  -- Lists
  '['        { L.Token L.LBrack _ }
  ']'        { L.Token L.RBrack _ }
  ','        { L.Token L.Comma _ }
  -- Types
  ':'        { L.Token L.Colon _ }
  '->'       { L.Token L.Arrow _ }
```

These tokens will terminals used while writing the grammar. They are first-class, and we can use them among other productions.

#### Semantic actions

With the definitions of our tokens, we can now begin writing the parser itself.
We can start with top-level definitions of format `let example = 0`.
This will be a simplistic definition for now, but we will elaborate on it shortly.
Note that production rules must be placed after the `%%` in the file, and you may delete the `empty` production after inserting the one below.

```happy
dec
  : let identifier '=' integer {}
```

We use `:` to describe the production to be parsed (called `dec`), denoting the tokens (defined above) to be parsed, separated by spaces.

And that's it.
Well, apart from the fact it does nothing useful but recognizes strings that match this definition or throws a parse error otherwise.

We usually want to perform an action simultaneously once something is successfully parsed.
We can put between the braces a semantic action that does something with the identifier and its integer.
For example, we could choose to create an interpreter or build a syntax tree, which is what we will do.

#### Abstract syntax trees

An abstract syntax tree (AST) describes the exact structure of the expressions we are parsing.
Before we continue parsing further, we should define the AST either in a new file and import it in our parser or at the trailer of the Happy file.
We will keep the AST in `Parser.y` for simplicity.

We will keep the AST small for now and grow it as needed.

```haskell
-- * AST

data Name a
  = Name a ByteString
  deriving (Foldable, Show)

data Type a
  = TVar a (Name a)
  deriving (Foldable, Show)

data Argument a
  = Argument a (Name a) (Maybe (Type a))
  deriving (Foldable, Show)

data Dec a
  = Dec a (Name a) [Argument a] (Maybe (Type a)) (Exp a)
  deriving (Foldable, Show)

data Exp a
  = EInt a Integer
  | EVar a (Name a)
  | EString a ByteString
  deriving (Foldable, Show)
```

Note the use of a polymorphic field in each constructor.
This is useful for storing extra parsing information in the tree, such as the range spanned by that node.
In practice, however, we will use `a ~ Range` while parsing, but having this field can be helpful if you plan to reuse your tree in other parts of your compiler and need to store more information.

A few words about each new data type introduced:

* A `Name` represents an identifier, such as a variable or type name. You could also define a `TypeName` in addition to `Name`, but for simplicity, we will stay only with `Name`.
* A `Type` contains the type annotation of a declaration, the right side of a type declaration (exercise proposed to the reader). MiniML will support functions, type names, the unit type, and lists. We will start with type names and add the remaining types later.
* An `Argument` stores information regarding a function parameter, such as its name and its type.
* A `Dec` represents a declaration consisting of a name, a possibly empty list of function arguments, an optional type annotation, and its body.
* An `Exp` describes each possible expression. Likewise, we start with simple ones: integers, variables, and strings and will add new ones later.

We will also create some utilities to use in the parser.

```haskell
-- | Build a simple node by extracting its token type and range.
unTok :: L.Token -> (L.Range -> L.TokenType -> a) -> a
unTok (L.Token tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2
```

The definition of `ìnfo` could be changed with a typeclass whose purpose is to match on the AST and extract its field, but here we take a shortcut.
If you want to understand better what's going on, I recommend checking the docs for [`First`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Monoid.html#t:First).

#### Parsing declarations

With it, we can improve our `dec` parser, as well as creating a few new parsers:

```happy
name :: { Name L.Range }
  : identifier { unTok $1 (\range (L.Identifier name) -> Name range name) }

dec :: { Dec L.Range }
  : let name '=' exp { Dec (L.tRange $1 <-> info $4) $2 [] Nothing $4 }

exp :: { Exp L.Range }
  : integer    { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name       { EVar (info $1) $1 }
  | string     { unTok $1 (\range (L.String string) -> EString range string) }
```

Happy provides the variables `$1`, `$2`, etc., that allow accessing the value of the parsed productions.
In the semantic action `EVar (info $1) $1`, for example, `$1` will have the type `Name Range`, which results from parsing `name`.

Likewise, in `dec`, `$1` will refer to the token `let`, `$2` to the parsed variable name, `$3` to the token `'='`, and `$4` to the parsed expression.

Notice that we now generalized `dec` to accept not only integers but any expression.
Furthermore, we create its range by compromising the start of `let` to the end of `exp`.

We've omitted the type annotation for the declaration, inserting `Nothing` in its place, as well as the function arguments, but we will fill it in shortly.

n.b.: We've used `::` here to give the type (between braces) of the production we're parsing.
This is optional and may be omitted if preferred.

At the top of your file, do the following substitution:

```diff
-%name parseMiniML
+%name parseMiniML dec
```

Happy uses the first non-terminal if the name is omitted.
In this case, we'd be able only to parse `name`, which is not what we want, so we make it use `dec` instead.

Let's also check that what we've made so far works. Open up GHCi and test it:

```haskell
>>> runAlex "let example = 42" parseMiniML
Right (Dec (Range {start = AlexPn 0 1 1, stop = AlexPn 16 1 17}) (Name (Range {start = AlexPn 4 1 5, stop = AlexPn 11 1 12}) "example") [] Nothing (EInt (Range {start = AlexPn 14 1 15, stop = AlexPn 16 1 17}) 42))
```

After stripping some of the range boilerplate, it's easy to check that this is what we wanted:

```haskell
Right (Dec _ (Name _ "example") [] Nothing (EInt _ 42))
```

If you are stuck and need some synchronization, you can find the code up to this point [here](https://gist.github.com/heitor-lassarote/fec2bb3feeb30f47be5a685937011917).

Moving on, let's continue parsing types and arguments for declarations.

```happy
type :: { Type L.Range }
  : name { TVar (info $1) $1 }

arguments :: { [Argument L.Range] }
  :                    { [] }
  | argument arguments { $1 : $2 }

argument :: { Argument L.Range }
  : '(' name          ')' { Argument (L.tRange $1 <-> L.tRange $3) $2 Nothing }
  | '(' name ':' type ')' { Argument (L.tRange $1 <-> L.tRange $5) $2 (Just $4) }
  | name                  { Argument (info $1) $1 Nothing }

dec :: { Dec L.Range }
  : let name arguments          '=' exp { Dec (L.tRange $1 <-> info $5) $2 $3 Nothing   $5 }
  | let name arguments ':' type '=' exp { Dec (L.tRange $1 <-> info $7) $2 $3 (Just $5) $7 }
```

Again, we start with only type names in `type` for now to keep it simple.

In `arguments`, the first production means that it's allowed to parse nothing, in which case we yield the empty list.

Some people like to place a comment instead when something is empty, like so:
```happy
arguments :: { [Argument L.Range] }
  : {- empty -}        { [] }
  | argument arguments { $1 : $2 }
```

I'm not personally a fan of doing this, but if this is clearer to you, feel free to do it. ;)

Even though this code works, it leads to some undesirable repetition.
Notably, we repeat two productions for accepting an optional type annotation.
Thankfully, Happy allows for _parameterized productions_, which you can think of as being similar to a function taking parsers as arguments.

Let us define a utility which we will call `optional`:

```happy
optional(p)
  :   { Nothing }
  | p { Just $1 }
```

The first alternative is to parse nothing, which we indicate by putting no productions in it and a semantic action returning `Nothing`.
The second alternative is to parse the given parser, called `p` here.

To parse the argument list, we will create another utility that parses a production `p` zero or more times, which we call `many`.

Additionally, it's recommended to use left recursions instead of right recursions, so we'd use `arguments argument` in the code above.
You would then call `reverse $1` (assuming `$1` is our list) or use a `Seq` to have the correct order.
This is because Happy can parse left recursions more efficiently. See [2.2 Parsing sequences](https://monlih.github.io/happy-docs/#_sec_sequences) in the Happy user guide for more information. So let's also change this!

```happy
many_rev(p)
  :           { [] }
  | many(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }
```

And now we can provide a definition of `dec` avoiding repetition:

```happy
typeAnnotation :: { Type L.Range }
  : ':' type { $2 }

argument :: { Argument L.Range }
  : '(' name optional(typeAnnotation) ')' { Argument (L.tRange $1 <-> L.tRange $4) $2 $3 }
  | name                                  { Argument (info $1) $1 Nothing }

dec :: { Dec L.Range }
  : let name many(argument) optional(typeAnnotation) '=' exp { Dec (L.tRange $1 <-> info $6) $2 $3 $4 $6 }
```

You may delete `arguments` with it as it's now unused.

Finally, let's make the parse capable of parsing zero or more declarations:

```happy
decs :: { [Dec L.Range] }
  : many(dec) { $1 }
```

And change `parseMiniML` again:

```diff
-%name parseMiniML dec
+%name parseMiniML decs
```

We can now open GHCi and check whether our changes work:

```haskell
>>> runAlex "let id (x : int) : int = x" parseMiniML
Right
  [ Dec _
    (Name _ "id")
    [Argument _ (Name _ "x") (Just (TVar _ (Name _ "int")))]
    (Just (TVar _ (Name _ "int")))
    (EVar _ (Name _ "x"))
  ]
```

The complete code for this section may be found [here](https://gist.github.com/heitor-lassarote/8c490c9b2772c764bce835505979cd59).

#### Parsing types

Besides parsing type names, we would like to parse functions, the unit type, and lists.
MiniML also contains sum and product types, but we will leave it as an exercise to the reader later.

Parsing a unit, parenthesis, or a list is trivial. First, we extend the `Type` AST with new productions:

```haskell
data Type a
  = TVar a (Name a)
  | TPar a (Type a)
  | TUnit a
  | TList a (Type a)
  | TArrow a (Type a) (Type a)
  deriving (Foldable, Show)
```

And we add it to our `type` production::

```happy
type :: { Type L.Range }
  : name           { TVar (info $1) $1 }
  | '(' ')'        { TUnit (L.tRange $1 <-> L.tRange $2) }
  | '(' type ')'   { TPar (L.tRange $1 <-> L.tRange $3) $2 }
  | '[' type ']'   { TList (L.tRange $1 <-> L.tRange $3) $2 }
```

What about the arrow type? Well, let's try it out. Add it to the `type` production:

```happy
  | type '->' type { TArrow (info $1 <-> info $3) $1 $3 }
```

And after compiling it, you should see the following message:

```
shift/reduce conflicts:  1
```

This message indicates that the parser has found an ambiguity in our grammar, which should be resolved.

To illustrate this problem, consider how this production could be parsed when given the input `int -> int -> int`.
The first way it could be parsed is `(int -> int) -> int`.
The second way it could be parsed is `int -> (int -> int)`, which we desire.

#### LR(1) conflicts

<!--
* shift/reduce, reduce/reduce
  * former is easier to resolve and usually is less serious
  * latter must be avoided because it usually specifies a malformed grammar
* shifting on operators often means left precedence, reducing means right precedence
* example: if-then and if-then-else problem is trivially resolved by shifting instead of reducing
-->

As previously mentioned, an LR(1) conflict indicates that the parser has found an ambiguity in the grammar.
They may be indicators of ill-formed grammars, and you should probably refactor it to avoid ambiguities. However, some cases can be reduced by explicitly telling Happy to either shift or reduce.
In general, there are two types of conflicts that you may find in Happy.

##### Shift/reduce conflicts

<!--
TODO: I would also appreciate an in-depth review from an LR(1) connoisseur.
-->

The first one is a shift/reduce conflict.
This is sometimes benign, and there are two well-known cases where it can be handled.

###### Operator precedence and associativity

The first one is regarding operator precedence and associativity, which is also the case we've had with type arrows.
Suppose the following snippet:

```happy
exp
  : exp '*' exp {}
  | exp '+' exp {}
  | integer     {}
```

Which is may also be represented by the following rules:

```text
exp -> exp '*' exp (1)
exp -> exp '+' exp (2)
exp -> integer     (3)
```

As well as the following states and actions:

<table>
  <tr>
    <td>State</td>
    <td>Position</td>
    <td>Action</td>
  </tr>
  <tr>
    <td>A</td>
    <td>
      <pre>exp -> exp . '*' exp (rule 1)
exp -> exp '*' exp . (rule 1)
exp -> exp . '+' exp (rule 2)</pre>
    </td>
    <td>
      <pre>'+'     shift, and enter state D (reduce using rule 1)
'*'     shift, and enter state C (reduce using rule 1)
%eof    reduce using rule 1</pre>
    </td>
  </tr>
  <tr>
    <td>B</td>
    <td>
      <pre>exp -> exp . '*' exp (rule 1)
exp -> exp . '+' exp (rule 2)
exp -> exp '+' exp . (rule 2)</pre>
    </td>
    <td>
      <pre>'+'     shift, and enter state D (reduce using rule 2)
'*'     shift, and enter state C (reduce using rule 2)
%eof    reduce using rule 2</pre>
    </td>
  </tr>
  <tr>
    <td>C</td>
    <td>
      <pre>exp -> exp '*' . exp (rule 1)</pre>
    </td>
    <td>
      <pre>integer shift, and enter state E
exp     goto state A</pre>
    </td>
  </tr>
  <tr>
    <td>D</td>
    <td>
      <pre>exp -> exp '+' . exp (rule 2)</pre>
    </td>
    <td>
      <pre>integer shift, and enter state E
exp     goto state B</pre>
    </td>
  </tr>
  <tr>
    <td>E</td>
    <td>
      <pre>exp -> integer .     (rule 3)</pre>
    </td>
    <td>
      <pre>'+'     reduce using rule 3
'*'     reduce using rule 3
%eof    reduce using rule 3</pre>
    </td>
  </tr>
</table>

n.b.: I omitted some states for brevity.

Remember that the dot (`.`) here indicates where the parser stopped while consuming input.
For example, the first position in state A means that the parser has just finished consuming the first expression and it's about to consume a star.

State A happens after we have consumed a `*`, while state B happens after consuming a `+`.

Suppose we use `1 + 2 * 3` as the input, and suppose that the parser has arrived at `1 + 2 . * 3` in state B (coming from state D).
It may either _shift_ to consume the `*` and go to state C or _reduce_ to accept that it has finished consuming the addition.

In other words, if it shifts, it will parse `1 + (2 * 3)`, giving priority to `*`, and if it reduces, it will parse `(1 + 2) * 3`, giving priority to `+`.
So if we want `*` to bind tighter than `+`, we want to reduce.

Likewise, what if we have `1 + 2 + 3` as the input?
Supposing that we stopped at `1 + 2 . + 3`, we enter state B again (coming from state D).
It may either be parsed as `(1 + 2) + 3` or `1 + (2 + 3)`.
If we shift, we make `+` right-associative, resulting in `1 + (2 + 3)`.
If we reduce, we make `+` left-associative, resulting in `(1 + 2) + 3`.

It means that an LR parser can use shift/reduce conflicts to either shift or reduce, allowing the programming to specify precedences and associativities for operators.

###### Dangling else

The second well-known case is the dangling-else problem, which is a conflict that arises when a language supports both if-then and if-then-else expressions (such as MiniML).
The expression `if a then if b then x else y` is ambiguous and may be parsed either as `if a then (if b then x else y)` or `if a then (if b then x) else y`.

Typically, the first interpretation is desirable.
To illustrate, let's see once again the parsing states:

```
exp -> if exp then exp . else exp
exp -> if exp then exp .
```

If we reduced, we would accept the input and get `if a then (if b then x) else b`. So we want to shift instead to continue parsing and get `if a then (if b then x else y)`.

##### Reduce/reduce conflicts

The second type of conflict is a reduce/reduce conflict, meaning that there are two or more possible non-terminals that the parser can accept.
This conflict is more severe than a shift/reduce conflict, and if you ever get it, you should refactor your grammar to eliminate it, as it's often not as trivially resolved as the case of a shift/reduce conflict.

#### Finding conflicts in Happy

Thankfully, Happy provides an easy way to find conflicts in grammars.
In your terminal, type the following:

```bash
$ happy -i src/Parser.y
```

You should see a message like so:

```
unused terminals: 16
Grammar info written to: src/Parser.info
shift/reduce conflicts:  1
```

It will generate `src/Parser.hs` and `src/Parser.info`, which you may remove after you are done reading them.
Now open `src/Parser.info`.

At the top of the file, it will list conflicts. In my `Parser.info`, I see the following:

```
state 32 contains 1 shift/reduce conflicts.
```

Now search for `State 32` in your file.
It should look like the following:

```
State 32

	type -> type . '->' type                            (rule 6)
	type -> type '->' type .                            (rule 6)

	'='            reduce using rule 6
	')'            reduce using rule 6
	']'            reduce using rule 6
	'->'           shift, and enter state 32
			(reduce using rule 6)
```

This block tells us that Happy will shift and not reduce when it sees that `->` as a look-ahead.
Remember that shifting makes the operator right-associative while reducing makes it left-associative.
In this case, we would like to make this token right-associative, so we have two ways of achieving this.

1. Use the `%shift` directive.

Happy allows us to indicate that a production should be shifted by writing `%shift` just before its semantic action.
Thus, our example could be modified like so:

```diff
-  | type '->' type { TArrow (info $1 <-> info $3) $1 $3 }
+  | type '->' type %shift { TArrow (info $1 <-> info $3) $1 $3 }
```

2. Indicate the precedence with `%left`, `%right`, or `%nonassoc`.

Alternatively, at the top of your grammar, somewhere before the `%%`, you may place the following directive:

```happy
%right '->'
```

This means that `->` will associate to the right.
To better illustrate this mechanism, let us define other precedences that will be useful once we start parsing expressions.

```happy
%right '->'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
```

This means that `|` binds less tightly than any other operator, followed by `&`.
The comparison operators are all non-associative\* and bind equally tight.
Addition and subtraction bind less tightly than division and multiplication.

\* Instead of shifting or reducing, it will cause a parse error.

There will be a few more precedences that we will indicate later, but for now, let's continue with this.

Let's test it and see how it goes:

```haskell
>>> runAlex "let f : int -> [int] -> () = x" parseMiniML
Right [Dec (Range {start = AlexPn 0 1 1, stop = AlexPn 30 1 31}) (Name (Range {start = AlexPn 4 1 5, stop = AlexPn 5 1 6}) "f") [] (Just (TArrow (Range {start = AlexPn 8 1 9, stop = AlexPn 26 1 27}) (TVar (Range {start = AlexPn 8 1 9, stop = AlexPn 11 1 12}) (Name (Range {start = AlexPn 8 1 9, stop = AlexPn 11 1 12}) "int")) (TArrow (Range {start = AlexPn 15 1 16, stop = AlexPn 26 1 27}) (TList (Range {start = AlexPn 15 1 16, stop = AlexPn 20 1 21}) (TVar (Range {start = AlexPn 16 1 17, stop = AlexPn 19 1 20}) (Name (Range {start = AlexPn 16 1 17, stop = AlexPn 19 1 20}) "int"))) (TUnit (Range {start = AlexPn 24 1 25, stop = AlexPn 26 1 27}))))) (EVar (Range {start = AlexPn 29 1 30, stop = AlexPn 30 1 31}) (Name (Range {start = AlexPn 29 1 30, stop = AlexPn 30 1 31}) "x"))]
``````

Simplifying this code a bit gives us:

```haskell
[ Dec _
  (Name _ "f")
  []
  (Just (TArrow _ (TVar _ (Name _ "int")) (TArrow _ (TList _ (TVar _ (Name _ "int"))) (TUnit _))))
  (EVar _ (Name _ "x"))
]
```

So we succesfully parsed the example as `int -> ([int] -> ())`.

The complete code for this section may be found [here](https://gist.github.com/heitor-lassarote/d9d6cd856584ab60cf7b3da4932dfc01).

#### Parsing expressions

Finally, let's move on to the last kind of production we want to parse: expressions.

For now, we can parse three simple expression types: integers, variables, and strings.
Expressions are often more complicated than this, as they may have function applications, list literals, local variable declarations, conditionals, etc.

To begin, let's enrich our `Exp` AST with the remainder of the nodes:

```haskell
data Operator a
  = Plus a
  | Minus a
  | Times a
  | Divide a
  | Eq a
  | Neq a
  | Lt a
  | Le a
  | Gt a
  | Ge a
  | And a
  | Or a
  deriving (Foldable, Show)

data Exp a
  = EInt a Integer
  | EVar a (Name a)
  | EString a ByteString
  | EUnit a
  | EList a [Exp a]
  | EPar a (Exp a)
  | EApp a (Exp a) (Exp a)
  | EIfThen a (Exp a) (Exp a)
  | EIfThenElse a (Exp a) (Exp a) (Exp a)
  | ENeg a (Exp a)
  | EBinOp a (Exp a) (Operator a) (Exp a)
  | EOp a (Operator a)
  | ELetIn a (Dec a) (Exp a)
  deriving (Foldable, Show)
```

A few words and an example about each production:
* `EInt`: An integer. `42`.
* `EVar`: A variable. `my_variable`.
* `EString`: A string. `"this is a string"`.
* `EUnit`: The unit literal, with type `()`. `()`.
* `EList`: A list literal. `[1, my_var, if true then 1 else 0`].
* `EPar`: An expression within parentheses. `(1 + foo bar)`.
* `EApp`: The application of an expression in a function. `my_func 1 "foo"`.
* `EIfThen`: A conditional producing a unit. `if true then print "Hello"`.
* `EIfThenElse`: A conditional. `if true then 1 else 2`.
* `ENeg`: The negation of an expression. `-(x + 5)`.
* `EBinOp`: The application of a binary operator to two expressions. `2 * 3 - 5`.
* `EOp`: A binary operator without operands. `(+) x 1`.
* `ELetIn`: Locally declares a variable. `let x = 5 in my_function x`.

We've previously defined a few primitives, namely `integer`, `name`, and `string`.
We may now add a few more simple productions.
Just so we're on the same page, I'll repeat the productions of `exp` that we already had.

```happy
exp :: { Exp L.Range }
  : integer                  { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name                     { EVar (info $1) $1 }
  | string                   { unTok $1 (\range (L.String string) -> EString range string) }
  | '(' ')'                  { EUnit (L.tRange $1 <-> L.tRange $2) }
  | '[' sepBy(exp, ',') ']'  { EList (L.tRange $1 <-> L.tRange $3) $2 }
  | '(' exp ')'              { EPar (L.tRange $1 <-> L.tRange $3) $2 }
```

The only thing new here is the introduction of a new `sepBy` function, which should separate various productions (`exp`) by a separator (`,`), just like a list in Haskell.
The definition of `sepBy` is provided below.

```happy
sepBy_rev(p, sep)
  :                     { [] }
  | sepBy(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }
```

Parsing such "atoms" is simple enough, but now we will find difficulties in parsing the other types of expressions, as they will have more complicated interactions.

##### Parsing function applications

Let's begin with `EApp`. You might want to add a new production defining it like so:

```happy
  | exp exp                  { EApp (info $1 <-> info $2) $1 $2 }
```

But immediately see that it will produce five shift/reduce conflicts.
The ambiguity comes from the fact that `a b c` could be parsed either as `(a b) c` or `a (b c)`.
If you analyze the output of `happy -i src/Parser.y`, you should see that it can either shift to another token or reduce it to a function application.

Remember, shifting asks Happy to continue parsing, making the output right-associative, while reducing asks Happy to accept what we have so far, making the output left-associative.
Since shifting is the default action, we will get the incorrect output with `let x = a b c`:

```haskell
Right
  [ Dec _ (Name _ "x") [] Nothing (EApp _ (EVar _ (Name _ "a")) (EApp _ (EVar _ (Name _ "b")) (EVar _ (Name _ "c"))))
  ]
```

Unfortunately, Happy doesn't have a `%reduce` directive, only a `%shift` directive.
Furthermore, using precedences here will not help us.

Instead, we will make an observation that will allow us to refactor our grammar in an intelligent way to resolve this ambiguity:
**The right side of a function application will always be an "atom"**.

To be more precise, ambiguities, in general, should appear every time we have a production such as `A B C ... exp` or `exp ... X Y Z`.
Had we placed another token to the left or the right of `exp exp`, such as `exp exp in`, the ambiguity would be eliminated (in this specific case).

The solution is to extract all "atoms" into one production, and use `atom` on the right side of the application:

```happy
exp :: { Exp L.Range }
  : exp atom                 { EApp (info $1 <-> info $2) $1 $2 }
  | atom                     { $1 }

atom :: { Exp L.Range }
  : integer                  { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name                     { EVar (info $1) $1 }
  | string                   { unTok $1 (\range (L.String string) -> EString range string) }
  | '(' ')'                  { EUnit (L.tRange $1 <-> L.tRange $2) }
  | '[' sepBy(exp, ',') ']'  { EList (L.tRange $1 <-> L.tRange $3) $2 }
  | '(' exp ')'              { EPar (L.tRange $1 <-> L.tRange $3) $2 }
```

This will cause Happy to build a chain `(((atom atom) atom) atom) atom` when parsing.
Note that even though the left side was stated to be an `exp`, eventually, we must reach an `atom` to terminate parsing.

With it, function applications should now be left-balanced.

##### Parsing conditional expressions

Let's see another case where we will get ambiguities.
Adding these rules to `exp` will indicate 11 shift/reduce conflicts:

```happy
  | if exp then exp          { EIfThen (L.tRange $1 <-> info $4) $2 $4 }
  | if exp then exp else exp { EIfThenElse (L.tRange $1 <-> info $6) $2 $4 $6 }
```

If you see the `Parser.info` file, most ambiguities come from two states. The first one is this:

```text
State 51

    exp -> exp . atom                                   (rule 12)
    exp -> if exp then exp .                            (rule 13)
    exp -> if exp then exp . else exp                   (rule 14)
```

We see that there is an ambiguity when parsing function applications and conditionals.
One instance of a conflicting parse is `if b then f x` when we arrive at `if b then f . x`.
Should it be parsed as `if b then (f x)` or `(if b then f) x`?

Shifting will cause the first parse to be accepted while reducing will cause the second parse to be accepted.
Remember, intuitively, shifting can be approximated as "continue parsing and see what we get" while reducing as "accept what we have so far".
This is why reducing inserts parenthesis right after `f` is parsed.

For now, just like we've split `exp` into `exp` and `atom` in the past, we can further split it:

```happy
exp :: { Exp L.Range }
  : expapp                   { $1 }
  | expcond                  { $1 }

expapp :: { Exp L.Range }
  : expapp atom              { EApp (info $1 <-> info $2) $1 $2 }
  | atom                     { $1 }

expcond :: { Exp L.Range }
  : if exp then exp          { EIfThen (L.tRange $1 <-> info $4) $2 $4 }
  | if exp then exp else exp { EIfThenElse (L.tRange $1 <-> info $6) $2 $4 $6 }
```

We use `expapp` to represent function applications, which will create a chain of `atom`s that represents the application.
Likewise, `expcond` represents the conditional expressions.
Since the left-hand side of an application can't be an `if` expression anymore, the conflict is eliminated.

It's important to note that extracting the conditional expressions from `exp` into their own `expcond` production is unnecessary. Still, it makes the grammar a bit more organized, in my opinion. :)

Be careful not to duplicate `atom` at the end of both `expapp` and `expcond`, or you will get reduce/reduce conflicts since Happy would not be able to figure out if an `atom` should be reduced using `expapp` or `expcond`.

With this, we now only have one shift/reduce conflict, which is the second ambiguity that needs to be resolved.
Recall that we've discussed before in this article that there is a conflict when parsing `if exp then exp` and `if exp then exp else exp`, which is the dangling-else problem.

In this case, we know that for an expression such as `if a then if b then c else d`, shifting will result in the correct answer of `if a then (if b then c else d)` while reducing will result in `if a then (if b then c) else d`.
Check the information file, where it will indicate that the conflict will either reduce with `if exp then exp` or shift to a new state to parse the `else exp` case.

The solution here is simple enough, just use the `%shift` directive:

```diff
-  : if exp then exp          { EIfThen (L.tRange $1 <-> info $4) $2 $4 }
+  : if exp then exp %shift   { EIfThen (L.tRange $1 <-> info $4) $2 $4 }
   | if exp then exp else exp { EIfThenElse (L.tRange $1 <-> info $6) $2 $4 $6 }
```

As a sanity check in GHCi, we can check the (prettified and simplified) output for the ambiguous expression:

```haskell
>>> runAlex "let x = if a then if b then c else d" parseMiniML
Right
  [ Dec _ (Name _ "x") [] Nothing           -- let x =
    (EIfThen _ (EVar _ (Name _ "a"))        --   if a then
      (EIfThenElse _ (EVar _ (Name _ "b"))  --     if b
      (EVar _ (Name _ "c"))                 --     then c
      (EVar _ (Name _ "d"))))               --     else d
  ]
```

You can find the source code up to this point [here](https://gist.github.com/heitor-lassarote/ebfddb9b354bb63dda2f952947d09f84).

#### Parsing negations

Now, let's make it, so we are able to negate expressions.
This case is pretty simple, just stick this definition in `exp`:

```happy
  | '-' exp                  { ENeg (L.tRange $1 <-> info $2) $2 }
```

This will also allow us to parse interesting cases, such as `-if True then 1 else 2` or `-succ 2`.
Try it in GHCi, both cases are valid Haskell code, and we also make it valid MiniML code now.

##### Parsing binary operators

Now let's move on with parsing binary operators, where Happy will make us unhappy again.

First, add new productions to parse each existing operator to `exp`:

```happy
  -- Arithmetic operators
  | exp '+'  exp             { EBinOp (info $1 <-> info $3) $1 (Plus (L.tRange $2)) $3 }
  | exp '-'  exp             { EBinOp (info $1 <-> info $3) $1 (Minus (L.tRange $2)) $3 }
  | exp '*'  exp             { EBinOp (info $1 <-> info $3) $1 (Times (L.tRange $2)) $3 }
  | exp '/'  exp             { EBinOp (info $1 <-> info $3) $1 (Divide (L.tRange $2)) $3 }
  -- Comparison operators
  | exp '='  exp             { EBinOp (info $1 <-> info $3) $1 (Eq (L.tRange $2)) $3 }
  | exp '<>' exp             { EBinOp (info $1 <-> info $3) $1 (Neq (L.tRange $2)) $3 }
  | exp '<'  exp             { EBinOp (info $1 <-> info $3) $1 (Lt (L.tRange $2)) $3 }
  | exp '<=' exp             { EBinOp (info $1 <-> info $3) $1 (Le (L.tRange $2)) $3 }
  | exp '>'  exp             { EBinOp (info $1 <-> info $3) $1 (Gt (L.tRange $2)) $3 }
  | exp '>=' exp             { EBinOp (info $1 <-> info $3) $1 (Ge (L.tRange $2)) $3 }
  -- Logical operators
  | exp '&'  exp             { EBinOp (info $1 <-> info $3) $1 (And (L.tRange $2)) $3 }
  | exp '|'  exp             { EBinOp (info $1 <-> info $3) $1 (Or (L.tRange $2)) $3 }
```

n.b.: Do NOT extract these operators to a new production and change it to `exp operator exp` because of a limitation in Happy. This will cause conflicts that will be pretty difficult to resolve.

Remember that we previously defined the precedences and associativities of each operator; otherwise, Happy would generate 168 shift/reduce conflicts. I will copy and paste them here yet again in case you've missed them:

```happy
%right '->'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
```

With it, we now have 12 shift/reduce conflicts.

You could observe that the left-hand side of an operator is always an `atom` and change it accordingly, but this will be no good.
Even though it would remove all of the shift/reduce conflicts, it would also have the side-effect that **the tree would always be right-balanced**.
You could live with it and properly balance the tree afterward (see the "rotation" method for `Dynamic infix operators with Alex and Happy` at the end of the article on how to do it). Still, in this case, it's possible to resolve this only by tweaking the grammar, so let's do it.

The reason we need to keep `exp operator exp` and not `atom operator exp` is because the first alternative can have ambiguities, such as parsing `1 + 2 + 3` as either `(1 + 2) + 3` or `1 + (2 + 3)`. The `%left`, `%nonassoc`, and `%right` directives will properly reduce, error, or shift based on the conflicts.
The second alternative would have no ambiguities and always build a right-balanced tree with a chain of `atom operator (atom operator atom)`, similar to function application.

Looking into `Parser.info` now, we see:

```text
State 81

    exp -> exp . '+' exp                                (rule 15)
    exp -> exp . '-' exp                                (rule 16)
    exp -> exp . '*' exp                                (rule 17)
    exp -> exp . '/' exp                                (rule 18)
    exp -> exp . '=' exp                                (rule 19)
    exp -> exp . '<>' exp                               (rule 20)
    exp -> exp . '<' exp                                (rule 21)
    exp -> exp . '<=' exp                               (rule 22)
    exp -> exp . '>' exp                                (rule 23)
    exp -> exp . '>=' exp                               (rule 24)
    exp -> exp . '&' exp                                (rule 25)
    exp -> exp . '|' exp                                (rule 26)
    expcond -> if exp then exp else exp .               (rule 30)
```

So the issue is not caused by the interaction between operators with other operators but rather between operators with conditional expressions.

An example is `if true then 0 else 1 + 2`.
Is this `if true then 0 else (1 + 2)` (shift) or `(if true then 0 else 1) + 2` (reduce)?

You have a few choices here. The first is to split the expressions into even more productions, the second is to add a `%shift` directive to the `if-then-else`, and the third is to add an associativity and precedence to the `else`.

I don't want to fiddle too much with the grammar or the LR parsing table, so for now, let's add a precedence to `else`:

```diff
+%right else
 %right '->'
 %left '|'
 %left '&'
```

This will cause the `else` keyword to extend as far to the right as possible, as it will have precedence over everything else.

##### Parsing an operator atom

This case is trivial, albeit annoying. Just shove these up in `atom`:

```happy
  -- Arithmetic operators
  | '(' '+' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Plus (L.tRange $2)) }
  | '(' '-' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Minus (L.tRange $2)) }
  | '(' '*' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Times (L.tRange $2)) }
  | '(' '/' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Divide (L.tRange $2)) }
  -- Comparison operators
  | '(' '=' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Eq (L.tRange $2)) }
  | '(' '<>' ')'             { EOp (L.tRange $1 <-> L.tRange $2) (Neq (L.tRange $2)) }
  | '(' '<' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Lt (L.tRange $2)) }
  | '(' '<=' ')'             { EOp (L.tRange $1 <-> L.tRange $2) (Le (L.tRange $2)) }
  | '(' '>' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Gt (L.tRange $2)) }
  | '(' '>=' ')'             { EOp (L.tRange $1 <-> L.tRange $2) (Ge (L.tRange $2)) }
  -- Logical operators
  | '(' '&' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (And (L.tRange $2)) }
  | '(' '|' ')'              { EOp (L.tRange $1 <-> L.tRange $2) (Or (L.tRange $2)) }
```

##### Parsing let-in

Finally, let's see `let...in...`, the final expression in our language.

Let's add a new case to `exp` and see what happens:

```happy
  | dec in exp               { ELetIn (info $1 <-> info $3) $1 $3 }
```

12 shift/reduce conflicts. Not cool. However, looking at `Parser.info`, this is a case that we are already familiar with, which is how conditional expressions interacted with operators, although now with `let...in...` instead of `if...then...else...`.

To illustrate, should `let a = b in a + b` be parsed as `(let a = b in a) + c` or `let a = b in (a + b)`? The second case is desirable, which is shifting when we reach `in`.

The solution is simply to add a precedence for `in`:

<!--
TODO: I'm not entirely sure if there will be any problems in mixing else and in with the same precedence. Maybe one of the reviewers knows better.
-->

```diff
-%right else
+%right else in
```

And that's it! The parser is now finished, and now you have a remarkable parser for an ML-like language that you can tweak as you want or use the code and insights gained here as inspiration to invent your grammar with Alex and Happy.

Try parsing the snippet at the introduction of this chapter:

```haskell
>>> runAlex "let the_answer : int =\n  let a = 20 in\n  let b = 1 in\n  let c = 2 in\n  a * c + b * c\n\nlet main (unit : ()) : () =\n  print (\"The answer is: \" + the_answer)" parseMiniML
Right [Dec (Range {start = AlexPn 0 1 1, stop = AlexPn 84 5 16}) (Name (Range {start = AlexPn 4 1 5, stop = AlexPn 14 1 15}) "the_answer") [] (Just (TVar (Range {start = AlexPn 17 1 18, stop = AlexPn 20 1 21}) (Name (Range {start = AlexPn 17 1 18, stop = AlexPn 20 1 21}) "int"))) (ELetIn (Range {start = AlexPn 25 2 3, stop = AlexPn 84 5 16}) (Dec (Range {start = AlexPn 25 2 3, stop = AlexPn 35 2 13}) (Name (Range {start = AlexPn 29 2 7, stop = AlexPn 30 2 8}) "a") [] Nothing (EInt (Range {start = AlexPn 33 2 11, stop = AlexPn 35 2 13}) 20)) (ELetIn (Range {start = AlexPn 41 3 3, stop = AlexPn 84 5 16}) (Dec (Range {start = AlexPn 41 3 3, stop = AlexPn 50 3 12}) (Name (Range {start = AlexPn 45 3 7, stop = AlexPn 46 3 8}) "b") [] Nothing (EInt (Range {start = AlexPn 49 3 11, stop = AlexPn 50 3 12}) 1)) (ELetIn (Range {start = AlexPn 56 4 3, stop = AlexPn 84 5 16}) (Dec (Range {start = AlexPn 56 4 3, stop = AlexPn 65 4 12}) (Name (Range {start = AlexPn 60 4 7, stop = AlexPn 61 4 8}) "c") [] Nothing (EInt (Range {start = AlexPn 64 4 11, stop = AlexPn 65 4 12}) 2)) (EBinOp (Range {start = AlexPn 71 5 3, stop = AlexPn 84 5 16}) (EBinOp (Range {start = AlexPn 71 5 3, stop = AlexPn 76 5 8}) (EVar (Range {start = AlexPn 71 5 3, stop = AlexPn 72 5 4}) (Name (Range {start = AlexPn 71 5 3, stop = AlexPn 72 5 4}) "a")) (Times (Range {start = AlexPn 73 5 5, stop = AlexPn 74 5 6})) (EVar (Range {start = AlexPn 75 5 7, stop = AlexPn 76 5 8}) (Name (Range {start = AlexPn 75 5 7, stop = AlexPn 76 5 8}) "c"))) (Plus (Range {start = AlexPn 77 5 9, stop = AlexPn 78 5 10})) (EBinOp (Range {start = AlexPn 79 5 11, stop = AlexPn 84 5 16}) (EVar (Range {start = AlexPn 79 5 11, stop = AlexPn 80 5 12}) (Name (Range {start = AlexPn 79 5 11, stop = AlexPn 80 5 12}) "b")) (Times (Range {start = AlexPn 81 5 13, stop = AlexPn 82 5 14})) (EVar (Range {start = AlexPn 83 5 15, stop = AlexPn 84 5 16}) (Name (Range {start = AlexPn 83 5 15, stop = AlexPn 84 5 16}) "c"))))))),Dec (Range {start = AlexPn 86 7 1, stop = AlexPn 154 8 41}) (Name (Range {start = AlexPn 90 7 5, stop = AlexPn 94 7 9}) "main") [Argument (Range {start = AlexPn 95 7 10, stop = AlexPn 106 7 21}) (Name (Range {start = AlexPn 96 7 11, stop = AlexPn 100 7 15}) "unit") (Just (TUnit (Range {start = AlexPn 103 7 18, stop = AlexPn 105 7 20})))] (Just (TUnit (Range {start = AlexPn 109 7 24, stop = AlexPn 111 7 26}))) (EApp (Range {start = AlexPn 116 8 3, stop = AlexPn 154 8 41}) (EVar (Range {start = AlexPn 116 8 3, stop = AlexPn 121 8 8}) (Name (Range {start = AlexPn 116 8 3, stop = AlexPn 121 8 8}) "print")) (EPar (Range {start = AlexPn 122 8 9, stop = AlexPn 154 8 41}) (EBinOp (Range {start = AlexPn 123 8 10, stop = AlexPn 153 8 40}) (EString (Range {start = AlexPn 123 8 10, stop = AlexPn 140 8 27}) "\"The answer is: \"") (Plus (Range {start = AlexPn 141 8 28, stop = AlexPn 142 8 29})) (EVar (Range {start = AlexPn 143 8 30, stop = AlexPn 153 8 40}) (Name (Range {start = AlexPn 143 8 30, stop = AlexPn 153 8 40}) "the_answer")))))]
```

Or, simplifying a bit:

```haskell
Right
  [ Dec _ (Name _ "the_answer") [] (Just (TVar _ (Name _ "int")))
    (ELetIn _ (Dec _ (Name _ "a") [] Nothing (EInt _ 20))
    (ELetIn _ (Dec _ (Name _ "b") [] Nothing (EInt _ 1))
    (ELetIn _ (Dec _ (Name _ "c") [] Nothing (EInt _ 2))
    (EBinOp _ (EBinOp _ (EVar _ (Name _ "a")) (Times _) (EVar _ (Name _ "c"))) (Plus _) (EBinOp _ (EVar _ (Name _ "b")) (Times _) (EVar _ (Name _ "c")))))))
  , Dec _ (Name _ "main") [Argument _ (Name _ "unit") (Just (TUnit _))] (Just (TUnit _))
    (EApp _ (EVar _ (Name _ "print")) (EPar _ (EBinOp _ (EString _ "\"The answer is: \"") (Plus _) (EVar _ (Name _ "the_answer")))))
  ]
```

To finish, let's just make one last change to our parser. At the top of the file, add the following:

```diff
 %lexer { lexer } { L.Token L.EOF _ }
+ %expect 0
```

This tells Happy that it should expect exactly zero shift/reduce conflicts.
Happy will always expect no reduce/reduce conflicts when this directive is present, no matter the provided value.

You can find the complete grammar [here](https://gist.github.com/heitor-lassarote/5f24f40c8625f25c6108ee59dc10e6d5).

#### Exercises

1. Support accessing list positions with `exp.(exp)` in your parser. For example, accessing the first element of a list would look like `my_list.(0)`.

2. Change the grammar to support patterns in declarations. Patterns are mainly similar to a subset of expressions, like a few of the atoms, such as integers, parentheses (although only accepting patterns inside), lists, strings, tuples (third exercise), and type constructors (second exercise).

Bonus: Support `match...with...` in your grammar.

3. Extend your lexer and parser to support the declaration of sum and product types.
```sml
type employee =
  | programmer of work_hours * favorite_language
  | manager of work_hours * [report]

type report = {project_name : string, tasks_completed : [string]}
```

You should be able to instantiate these datatypes:

```sml
let alex = programmer (8, haskell)
let happy = manager (8, [{project_name = "MiniML", tasks_completed = ["lexer", "parser"]}])
```

Access fields using the `.` operator:

```sml
type person = {name : string, year : int, height : double (* meters *)}
let bach = {name = "Bach", year = 1685, height = 1.8}
let bach's_age : person = 2022 - bach.age
```

4. Extend your lexer and parser to support tuples in types and expressions.
```sml
let example : int * int * string = 0, 1, "hello"
```

Be careful with conflicts:

```sml
let danger : int * (int * string) = 0, let x = 1 in x, "hello"
```

Should be parsed as:

```sml
let danger : int * (int * string) = (0, let x = 1 in (x, "hello"))
```

And NOT like:

```sml
let danger : int * (int * string) = (0, (let x = 1 in x), "hello")
```

Note that there are places where you should enforce the use of parenthesis. For instance, `[0, 1]` is a list with two elements, but `[(0, 1)]` is a list with one pair. Likewise, in record creations, you should enforce parenthesis.
Alternatively, you may use a semicolon (`;`) in the list and record syntaxes.

## Conclusion

In this tutorial, we demonstrated how to build a lexer using Alex to turn lexemes into tokens, using start codes and a user state adequate for MiniML. We then integrated Alex with Happy, where we created a grammar, resolved LR conflicts, and built an abstract syntax tree.

With it, we hope that you can now use these tools to make valuable things while understanding how to overcome the various challenges that arise when creating grammars.

## Further reading

<!--
* [X] copy and paste links to blog/Twitter etc. from previous articles
-->

If you liked this article, you might also enjoy these resources:

* [How to Implement an LR(1) Parser](https://serokell.io/blog/how-to-implement-lr1-parser).
* [Dynamic infix operators with Alex and Happy](https://gist.github.com/heitor-lassarote/b20d6da0a9042d31e439befb8c236a4e).
* [Parser Combinators in Haskell](https://serokell.io/blog/parser-combinators-in-haskell).
* GHC's [Alex](https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser/Lexer.x) and [Happy](https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y) files.
* APPEL, A. W. (1998). _Modern Compiler Implementation in ML_, Cambridge University Press.

For more Haskell tutorials, you can check out our [Haskell articles](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Dev](https://dev.to/serokell).

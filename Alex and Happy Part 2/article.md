# Parsing with Haskell (Part 2): Parsing with Happy

<!--
* [X] refer to LR(1) article
* [X] link to its user guide
-->

This is the second and final part of the Parsing with Haskell series.
If you haven't read the first part yet, you can find it [here](https://serokell.io/blog/lexing-with-alex).

In this article, we will use [Happy](https://www.haskell.org/happy/) to generate a parser that will consume the token stream from the lexer created by Alex.

As with Alex, our goal is to provide enough information to be productive.
Hence, we won't cover all the features of Happy.
Please refer to the [Happy User Guide](https://monlih.github.io/happy-docs/) for more information.

This tutorial was written using GHC version 9.0.2, Stack resolver LTS-19.8, Alex version 3.2.7.1, and Happy version 1.20.0.

## Our grammar: MiniML

We discussed our grammar in the previous article, but let's reintroduce it here.

MiniML (read as "minimal") is a small grammar based on [ML](https://en.wikipedia.org/wiki/ML_(programming_language)).
As the name suggests, it has the minimal syntax needed to get you started with Alex and Happy.

By the end of this tutorial, you will be able to parse programs such as the following:

```sml
let the_answer : int =
  let a = 20 in
  let b = 1 in
  let c = 2 in
  a * c + b * c

let main (unit : ()) : () =
  print ("The answer is: " + the_answer)
```

Some of the features that this tutorial will cover are:

* Variables, constants, function declarations, function applications, local bindings, and operators.
* Some simple types, as well as arrow types (functions).
* Conditional expressions: if-then-else and if-then.

In addition, exercises will guide you through extending the language with patterns, list access, and pattern matching. The first part also introduced block comments (in the main text) and rational numbers and line comments (in exercises).

## Happy vs. Megaparsec

Happy is a [parser generator](https://web.mit.edu/6.005/www/fa15/classes/18-parser-generators/) that generates LALR(1) (bottom-up) parsers, which are more performant yet less powerful than regular LR(1) parsers.

Meanwhile, Haskell libraries such as Megaparsec work best on LL(1) or LL(∞) grammars. They're implemented as recursive descent parsers (top-down parsers) with backtracking.

As such, these libraries have caveats like not being able to handle [left recursion](https://en.wikipedia.org/wiki/Left_recursion). Their performance can also exponentially degrade due to backtracking.

While comparing the implementations of both parsing techniques is beyond the scope of this post, you can read these two blog posts for more information: [From recursive descent to LR parsing](https://www.abubalay.com/blog/2021/12/31/lr-control-flow) and [Which Parsing Approach?](https://tratt.net/laurie/essays/entries/which_parsing_approach.html).

Another difference between these libraries is that Happy is a parser generator.
Instead of writing Haskell (`.hs`) files like you do with Megaparsec, you write Happy (`.y`) files.
They, in turn, generate Haskell files with parsers for the described grammars.

To sum it up, here are some pros and cons of Happy in comparison to Megaparsec.

**Pros:**
* Parser generators report ambiguous grammars to the user, creating greater trust in the parser's output and saving from debugging headaches.
* Bottom-up parsers can handle left recursion, while top-down parsers can't.
* The performance will be more predictable, as Megaparsec can cause exponential time complexity with the use of `try`. In addition, Happy's generated LALR algorithm has better known complexity constraints.
* Dealing with operator precedence and associativity requires less labor. In Megaparsec, there are standard tools like [`makeExprParser`](https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Monad-Combinators-Expr.html#v:makeExprParser) to deal with them, but once you need something that is non-standard, you'll need to write more non-trivial parser rules by hand.

**Cons:**
* Less idiomatic, as you need to write Happy grammars instead of Haskell code.
* Less flexible since you can't change Happy’s algorithm. Meanwhile, parser combinators enable you to easily create your own functions.

## How Happy's parsers work

An in-depth explanation of how an LR(1) parser works is well beyond the scope of this tutorial. For more info on this, we recommend you read our article on [how to implement an LR(1) Parser](https://serokell.io/blog/how-to-implement-lr1-parser).
We will, however, introduce a few basic concepts required to understand this article better.

### Formal grammar

When talking about parsing, we generally imply we're parsing some language, a formal language in the case of programming languages.
But what is a language?

Formal language theory offers a very simple answer: a language is a (likely infinite) set of all grammatically correct sentences.
Now, the word "sentence" is used in a very general sense here – a complete MiniML program would still constitute a sentence.

Well, but how do you describe an infinite set of sentences?
You do that by specifying how how to build any arbitrary sentence.

This description is called the formal grammar of the language.
The usual approach uses string substitutions to construct a sentence.
The process of constructing a sentence itself is called the derivation process, which is represented as a sequence of substitutions called a derivation chain.
We can refer to either as **derivation**.

We start with a special symbol called the start non-terminal.
Then, we find and replace symbols according to our substitution rules.
Once we have replaced everything we could have replaced, we stop.

The symbols to be replaced are called **non-terminals**, and the symbols they are ultimately replaced by are called **terminals**.
The symbols that we choose for non-terminals do not occur in the language, although the things they denote do.
Terminals represent the lexical items of the language – you can think of them being the "words" of the language.
They are called so because derivation terminates on them.

The rules for substituting symbols are called **production rules**.
They are the rules that define the grammar (and, by extension, the language).
In formal language theory, we usually write them as `alpha -> beta`, where `alpha` is called the production head, and `beta` the production body.
In each step of derivation, some head is replaced by its corresponding body.

### Parsing with a grammar

Parsing is, in a sense, a process inverse to derivation. Instead of building an arbitrary sentence, we're given a sentence and tasked with recovering the derivation that produced it.

There are two broad approaches to accomplish that. We could try to run the derivation process forwards, but at each step choose the substitution that would get us closer to the input string. This is top-down parsing. Not all grammars work well with this approach, but it is arguably more straightforward.

Or we could run the derivation process in reverse by finding a production rule whose body matches some of the input string. We do that until we're back at the starting non-terminal. This is bottom-up parsing. It works well with more grammars (still not all of them, though), and that's the approach Happy adheres to.

In our case, we'll generate a bottom-up parser that will build an **abstract syntax tree** (AST), which is a data structure that describes the exact grammatical structure of the input.

For an input like `-42 * f x`, we'll get a tree that looks like this:

```haskell
EBinOp _ (ENeg _ (EInt _ 42)) (Times _) (EApp _ (EVar _ "f") (EVar "x"))
```

It can be better visualized like this:

```haskell
                          EBinOp _
                         /   |    \
                  ENeg _     |     EApp _
                 /       Times _  /      \
        EInt _ 42                /        \
                       EVar _ "f"          EVar _ "x"
```

So that Happy could generate the parser, we need to provide a grammar in terms of terminals, non-terminals, and production rules.

The terminals are going to be the tokens that were produced by Alex, and they will correspond to the leaves of the abstract syntax tree (`EInt`, `Times`, and `EVar` in the example above).

This is how we define terminals in Happy:

```happy
%token
  identifier { L.RangedToken (L.Identifier _) _ }
  string     { L.RangedToken (L.String _) _ }
  integer    { L.RangedToken (L.Integer _) _ }
  let        { L.RangedToken L.Let _ }
  in         { L.RangedToken L.In _ }
  '('        { L.RangedToken L.LPar _ }
  ')'        { L.RangedToken L.RPar _ }
  -- And others...
```

Likewise, non-terminals will correspond to the internal nodes of the abstract syntax tree.
`EBinOp`, `ENeg`, and `EApp` are examples of nodes parsed from non-terminals.

Finally, this is how production rules look like in Happy:

```happy
exp :: { Exp L.Range }
  : integer                  { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name                     { EVar (info $1) $1 }
  | '(' exp ')'              { EPar (L.rtRange $1 <-> L.rtRange $3) $2 }
```

To break down this syntax, let's look at each individual component.

**Head**

`exp :: { Exp L.Range }` is the head of the production. With it, you can use `exp` to refer to this non-terminal in other parts of the grammar. The type signature is optional, meaning that only `exp` without ` :: { Exp L.Range }` would also be accepted.

In context-free grammars (CFGs), which are what Happy supports, production heads always consist of a single non-terminal.
There also may be multiple production rules with the same head, which is how we encode multiple choices.

**Body**

Each sequence of symbols after `:` or after `|` indicates how to form a sentence of the language. For instance, `'(' exp ')'` is an expression wrapped in parentheses.

The pipe (`|`) represents an alternative (an _or_). This means that the parser may parse any of the given bodies, accepting the one that matches.

The body of a production may also be empty, meaning that `myNonTerminal : { action }` is accepted.

**Action**

Like in Alex, any expression betwen braces `{ }` indicates what the parser should do once it successfully parses that rule, such as how to interpret what it has parsed or how to build a data structure with it. We'll use these actions to build our AST.

Happy provides the symbols `$1`, `$2`, `$3`, etc., that allow accessing the semantic value of the parsed symbols.
For terminals, the semantic values correspond to tokens, and for non-terminals, they correspond to the value that the corresponding action returned.

In the semantic action `EVar (info $1) $1`, for example, `$1` will have the type `Name Range`, which results from parsing `name`.
Likewise, in `'(' exp ')'`, `$1` will refer to the token `(`, `$2` to the parsed `exp` expression, and `$3` to the token `')'`.

### Look-ahead

The **look-ahead** represents the sequence of symbols that will be consumed next by the parser.
Happy generates LALR(1) parsers, which means their look-ahead is 1, so they can see one incoming symbol.

### Components of a Happy parser

Similarly to lexers generated by Alex, a Happy parser also contains a state machine.
This finite-state machine is used to make parsing decisions.
It's a [deterministic finite automaton](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) that we call an **LR automaton**.

At the same time, the parser contains a stack with all the tokens consumed during parsing, making it a [pushdown automaton](https://en.wikipedia.org/wiki/Pushdown_automaton).
The parser generates an action table describing how to make parsing decisions while consuming the input stream, in accordance to the current stack and look-ahead.

Here are the possible actions in the action table:

* **Shift** makes the parser push (shift) the state corresponding to the current input symbol on the top of its internal stack and continue parsing.
* **Reduce** indicates that the parser has accepted a production, and so it will pop states from the stack, push the state corresponding to the production head on the stack, and run an action with the consumed input.
* **Goto** causes the parser to change states without consuming input.
* **Accept** is the last action performed by the parser when it has seen all valid input. Accepting means that the program was successfully parsed, and it normally happens when the parser has reached EOF.

<details>
<summary>Interactive parser demo</summary>
If you'd like to see an interactive parser showing a step-by-step presentation on how it works, you may be interested in Nikolay Yakimov's <a href=https://lierdakil.github.io/parser-demo/>parser-demo</a>. Change the dropdown menu to "LALR", press "Run", and use the arrows to step through the parsing process.

In the demo, the current stack is on the left, the current tree is on the right, the action table is in the center, the grammar is on the bottom, and the input string is at the top.
You can input arbitrary strings and write arbitrary grammars, but there's no lexer, so the input string is a sequence of terminals separated by whitespace.

The control table is divided in two parts:
* To the left, there is the action table with the terminals.
* To the right, there is the goto table, which is used to decide what state to push at the top of the stack after reducing.
</details>

When working with Happy, shifting and reducing are two core concepts which we will come into contact later.

But enough with theory, let's go to the practice and learn more concepts as we go through it.

## Our first parser

Below is a minimal definition of a grammar file that Happy can compile.
We've inserted an extra `empty` rule that we'll delete later.
It's only there to make Happy happy.

Make sure to put it in a new `Parser.y` file.

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
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%%

empty : {}  -- only to get the file compiling; we will remove this

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}
```

Happy files have a similar structure to Alex files, where we insert Haskell definitions at the top and the bottom of the file, with the parser definition in the middle.
Likewise, anything between `{` and `}` is going to be inlined as well.

Let's quickly visit each part of this file, from top to bottom.
* The top of the file (header) contains the code that will generate the header of our Haskell file. Happy will generate the primary parsing function, `parseMiniML`, which we export.
  * `DeriveFoldable` allows us to write `deriving (Foldable)`, which will be helpful later.
  * We also import some extra things that will be useful in utilities later.
* The following lines instruct Happy on how exactly to generate the parser. The things we should provide to it include:
  * One or more names for our parser. The syntax should be `%name PARSER_NAME [PRODUCTION]`. If `PRODUCTION` is not given, it uses the first non-terminal that appears in the file.
  * What is the type of our tokens with `%tokentype`.
  * Which function should it call if there is a parse error with `%error`.
  * What is the monad that should be used and its canonical operations with `%monad` (we reuse `Alex` for simplicity).
  * Which function lexes tokens and how to extract the EOF token with `%lexer`.
* `%%`, after which we must specify the production rules for the grammar.
* The bottom of the file (trailer) to include more Haskell definitions, such as auxiliary files and the AST that will be built by the parser. You can also define those in another module if you prefer.

<details>
  <summary>A note about the <code>StrictData</code> language extension</summary>
  
  There is one pitfall you should be aware of while using Happy: you should not compile the parser module using <code>StrictData</code>, as it may cause an internal Happy exception.

  If you'd like to make your data types strict, you may either use bangs (<code>!</code>) in your data type or define them in another module.

  If you've enabled <code>StrictData</code> in your Cabal file, you may place a <code>NoStrictData</code> pragma in your Happy module.
</details>

### Tokens

Since Happy operates on tokens, we must tell it how to match each token.
This is done by using `%token`, which must be placed before `%%` in the file.
For each token, on the left side, we put an alias that we'll use to refer to our terminals.
On the right side, we put a Haskell pattern between braces that indicates how Happy should match the Alex tokens.

```happy
%token
  -- Identifiers
  identifier { L.RangedToken (L.Identifier _) _ }
  -- Constants
  string     { L.RangedToken (L.String _) _ }
  integer    { L.RangedToken (L.Integer _) _ }
  -- Keywords
  let        { L.RangedToken L.Let _ }
  in         { L.RangedToken L.In _ }
  if         { L.RangedToken L.If _ }
  then       { L.RangedToken L.Then _ }
  else       { L.RangedToken L.Else _ }
  -- Arithmetic operators
  '+'        { L.RangedToken L.Plus _ }
  '-'        { L.RangedToken L.Minus _ }
  '*'        { L.RangedToken L.Times _ }
  '/'        { L.RangedToken L.Divide _ }
  -- Comparison operators
  '='        { L.RangedToken L.Eq _ }
  '<>'       { L.RangedToken L.Neq _ }
  '<'        { L.RangedToken L.Lt _ }
  '<='       { L.RangedToken L.Le _ }
  '>'        { L.RangedToken L.Gt _ }
  '>='       { L.RangedToken L.Ge _ }
  -- Logical operators
  '&'        { L.RangedToken L.And _ }
  '|'        { L.RangedToken L.Or _ }
  -- Parenthesis
  '('        { L.RangedToken L.LPar _ }
  ')'        { L.RangedToken L.RPar _ }
  -- Lists
  '['        { L.RangedToken L.LBrack _ }
  ']'        { L.RangedToken L.RBrack _ }
  ','        { L.RangedToken L.Comma _ }
  -- Types
  ':'        { L.RangedToken L.Colon _ }
  '->'       { L.RangedToken L.Arrow _ }
```

These aliases to tokens are the terminals we'll use while writing the grammar. They are first-class, and we can use them among other symbols.

### First production rule

Now that we have the definitions of our tokens, we can now begin writing the parser itself.

We'll start with top-level definitions of format `let example = 0`.
This will be a simplistic definition for now, but we will elaborate on it shortly.

Note that production rules must be placed after `%%` in the file.
You may delete the `empty` production after inserting the one below.

```happy
dec
  : let identifier '=' integer {}
```

We use `:` to describe the production body to be parsed (whose production head is `dec`). We denote the tokens (defined [above](https://serokell.io/blog/parsing-with-happy#tokens)) to be parsed, separated by spaces.

And that's it, apart from the fact that the rule does nothing useful.
It recognizes strings that match this definition or throws a parse error otherwise.

Usually, we want to perform an action once something is successfully parsed.
To do that, we can put a semantic action between the braces that does something with the identifier and its integer.
For example, we could choose to create an interpreter or build a syntax tree.
We'll cover how to do the latter in this tutorial.

### Abstract syntax trees

Before we continue parsing further, we should either define the AST in a new file and import it in our parser or define it at the trailer of the Happy file.
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
This is useful for storing extra parsing information in the tree.
In practice, range is the only thing we'll store there, but having this field can be helpful if you plan to reuse the tree in other parts of your compiler and need to store more information.

A few words about each new data type introduced:

* `Name` represents an identifier, such as a variable or a type name. You could also define a `TypeName` in addition to `Name`, but for simplicity, we will stay only with `Name`.
* `Type` contains the type annotation of a declaration. MiniML will support function types, type names, the unit type, and lists. We'll start with type names and add the remaining types later.
* `Argument` stores information regarding a function parameter, such as its name and its type.
* `Dec` represents a declaration, which consists of a name, a possibly empty list of function arguments, an optional type annotation, and the body of the declaration.
* `Exp` describes each possible expression. We'll start with simple ones – integers, variables, and strings – and add new ones later.

We'll also create some utility functions for the parser.

```haskell
-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

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

The definition of `ìnfo` could be switched with a typeclass whose purpose is to match on the AST and extract its field, but here we take a shortcut.
If you want to better understand what's going on, I recommend checking the docs for [`First`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Monoid.html#t:First).

### Parsing declarations

With the AST in hand, we can improve our `dec` parser and create a few new parsers:

```happy
name :: { Name L.Range }
  : identifier { unTok $1 (\range (L.Identifier name) -> Name range name) }

dec :: { Dec L.Range }
  : let name '=' exp { Dec (L.rtRange $1 <-> info $4) $2 [] Nothing $4 }

exp :: { Exp L.Range }
  : integer    { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name       { EVar (info $1) $1 }
  | string     { unTok $1 (\range (L.String string) -> EString range string) }
```

Notice that we have generalized `dec` to accept not only integers but any expression.
Furthermore, we make it so that `dec`'s range starts from the start point of `let` and ends at the end point of `exp`.

We've omitted the type annotation for the declaration (inserting `Nothing` in its place) as well as the function arguments, but we'll fill those in shortly.

At the top of your file, do the following substitution:

```diff
-%name parseMiniML
+%name parseMiniML dec
```

Happy uses the first non-terminal if the name is omitted.
In this case, we'd be able only to parse `name`, which is not what we want, so we make it use `dec` instead.

Let's check that what we've made works so far.

Open up GHCi and test it:

```haskell
>>> runAlex "let example = 42" parseMiniML
Right (Dec (Range {start = AlexPn 0 1 1, stop = AlexPn 16 1 17}) (Name (Range {start = AlexPn 4 1 5, stop = AlexPn 11 1 12}) "example") [] Nothing (EInt (Range {start = AlexPn 14 1 15, stop = AlexPn 16 1 17}) 42))
```

After stripping some of the range boilerplate, it's easy to see that this is what we wanted:

```haskell
Right (Dec _ (Name _ "example") [] Nothing (EInt _ 42))
```

If you are stuck and need some synchronization, you can find the code up to this point [here](https://gist.github.com/heitor-lassarote/fec2bb3feeb30f47be5a685937011917).

Moving on, let's continue with parsing types and arguments of declarations.

```happy
type :: { Type L.Range }
  : name { TVar (info $1) $1 }

arguments :: { [Argument L.Range] }
  :                    { [] }
  | argument arguments { $1 : $2 }

argument :: { Argument L.Range }
  : '(' name          ')' { Argument (L.rtRange $1 <-> L.rtRange $3) $2 Nothing }
  | '(' name ':' type ')' { Argument (L.rtRange $1 <-> L.rtRange $5) $2 (Just $4) }
  | name                  { Argument (info $1) $1 Nothing }

dec :: { Dec L.Range }
  : let name arguments          '=' exp { Dec (L.rtRange $1 <-> info $5) $2 $3 Nothing   $5 }
  | let name arguments ':' type '=' exp { Dec (L.rtRange $1 <-> info $7) $2 $3 (Just $5) $7 }
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
Thankfully, Happy allows for _parameterized productions_, which you can think of as being similar to functions taking parsers as arguments.

Let us define a utility called `optional`:

```happy
optional(p)
  :   { Nothing }
  | p { Just $1 }
```

The first alternative is to parse nothing, which we indicate by putting no productions in it and a semantic action returning `Nothing`.
The second alternative is to parse the given parser, called `p` here.

To parse the argument list, we will create another utility that parses a production `p` zero or more times, called `many`.

Additionally, it's recommended to use left recursion instead of right recursion, so we'd use `arguments argument` in the code above.
You would then call `reverse $1` (assuming `$1` is our list) or use a `Seq` to have the correct order.
This is because Happy can parse left recursions more efficiently – LR requires O(n) stack space for right recursion and only O(1) for left recursion. See [2.2 Parsing sequences](https://monlih.github.io/happy-docs/#_sec_sequences) in the Happy user guide for more information. So let's also change this!

```happy
many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }
```

And now we can provide a definition of `dec` that avoids repetition:

```happy
typeAnnotation :: { Type L.Range }
  : ':' type { $2 }

argument :: { Argument L.Range }
  : '(' name optional(typeAnnotation) ')' { Argument (L.rtRange $1 <-> L.rtRange $4) $2 $3 }
  | name                                  { Argument (info $1) $1 Nothing }

dec :: { Dec L.Range }
  : let name many(argument) optional(typeAnnotation) '=' exp { Dec (L.rtRange $1 <-> info $6) $2 $3 $4 $6 }
```

You may delete `arguments` since it's unused now.

Finally, let's make the parser capable of parsing zero or more declarations:

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

### Parsing types

Besides parsing type names, we would like to parse functions, the unit type, and lists.

Parsing a unit, parenthesis, or a list is trivial. First, we extend the `Type` AST with new nodes:

```haskell
data Type a
  = TVar a (Name a)
  | TPar a (Type a)
  | TUnit a
  | TList a (Type a)
  | TArrow a (Type a) (Type a)
  deriving (Foldable, Show)
```

And then we add the corresponding `type` productions to the grammar:

```happy
type :: { Type L.Range }
  : name           { TVar (info $1) $1 }
  | '(' ')'        { TUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '(' type ')'   { TPar (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '[' type ']'   { TList (L.rtRange $1 <-> L.rtRange $3) $2 }
```

n.b.: Having a `TPar` node is not necessary because from the AST alone we can figure out the correct precedence of operators and avoid redundant parentheses, and you could have used `$2` in its semantic action instead, but we opted to leave it here for consistency.

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

### Conflicts

<!--
* shift/reduce, reduce/reduce
  * former is easier to resolve and usually is less serious
  * latter must be avoided because it usually specifies a malformed grammar
* shifting on operators often means left precedence, reducing means right precedence
* example: if-then and if-then-else problem is trivially resolved by shifting instead of reducing
-->

As previously mentioned, a conflict indicates that the parser has found an ambiguity in the grammar.
Conflicts may be indicators of ill-formed grammars, and you should probably refactor them to avoid ambiguities.
However, some cases can be resolved by explicitly telling Happy to either shift or reduce.

In general, there are two types of conflicts that you may encounter in Happy: shift/reduce and reduce/reduce.

#### Shift/reduce conflicts

<!--
TODO: I would also appreciate an in-depth review from an LR(1) connoisseur.
-->

A shift/reduce conflict can sometimes be benign, and there are two well-known cases where it can be handled.

##### Operator precedence and associativity

The first case concerns operator precedence and associativity, which is also the case we've had with type arrows.

Suppose the following snippet:

```happy
exp
  : exp '*' exp {}
  | exp '+' exp {}
  | integer     {}
```

Which may also be represented by the following rules:

```text
%start_parseExp -> exp (0)
exp -> exp '*' exp     (1)
exp -> exp '+' exp     (2)
exp -> integer         (3)
```

As well as the following states and actions:

<table>
  <tr>
    <td>State</td>
      <td>Position (signified by <code>.</code>)</td>
    <td>Action</td>
  </tr>
  <tr>
    <td>0</td>
    <td><pre>%start_parseExp -> . exp (rule 0)</pre></td>
    <td>
      <pre>integer shift, and enter state 3
exp     goto state 4</pre>
    </td>
  </tr>
  <tr>
    <td>1</td>
    <td><pre>exp -> . exp '*' exp (rule 1)</pre></td>
    <td><pre>integer shift, and enter state 3
exp     goto state 2</pre>
    </td>
  </tr>
  <tr>
    <td>2</td>
    <td>
      <pre>exp -> exp . '*' exp (rule 1)
exp -> exp . '+' exp (rule 2)</pre>
    </td>
    <td>
      <pre>'+'     reduce using rule 3
'*'     reduce using rule 3
%eof    reduce using rule 3</pre>
    </td>
  </tr>
  <tr>
    <td>3</td>
    <td>
      <pre>exp -> integer .     (rule 3)</pre>
    </td>
    <td>
      <pre>'+'     reduce using rule 3
'*'     reduce using rule 3
%eof    reduce using rule 3</pre>
    </td>
  </tr>
  <tr>
    <td>4</td>
    <td>
      <pre>%start_parseExp -> exp (rule 0)
exp -> exp . '*' exp (rule 1)
exp -> exp . '+' exp (rule 2)</pre>
    </td>
    <td>
      <pre>'+'     shift, and enter state 5
'*'     shift, and enter state 6
%eof    accept</pre>
    </td>
  </tr>
  <tr>
    <td>5</td>
    <td>
      <pre>exp -> exp '+' . exp (rule 2)</pre>
    </td>
    <td>
      <pre>integer shift, and enter state 3
exp     goto state 8</pre>
    </td>
  </tr>
  <tr>
    <td>6</td>
    <td>
      <pre>exp -> exp '*' . exp (rule 1)</pre>
    </td>
    <td>
      <pre>integer shift, and enter state 3
exp     goto state 7</pre>
    </td>
  </tr>
  <tr>
    <td>7</td>
    <td>
      <pre>exp -> exp . '*' exp (rule 1)
exp -> exp '*' exp . (rule 1)
exp -> exp . '+' exp (rule 2)</pre>
    </td>
    <td>
      <pre>'+'     shift, and enter state 5 (reduce using rule 1)
'*'     shift, and enter state 6 (reduce using rule 1)
%eof    reduce using rule 1</pre>
    </td>
  </tr>
  <tr>
    <td>8</td>
    <td>
      <pre>exp -> exp . '*' exp (rule 1)
exp -> exp . '+' exp (rule 2)
exp -> exp '+' exp . (rule 2)</pre>
    </td>
    <td>
      <pre>'+'     shift, and enter state 5 (reduce using rule 2)
'*'     shift, and enter state 6 (reduce using rule 2)
%eof    reduce using rule 2</pre>
    </td>
  </tr>
</table>

Remember that the dot (`.`) here indicates where the parser stopped while consuming input.
For example, the first position in state 7 means that the parser has just finished consuming the first expression and it's about to consume a star.

State 7 happens after we have consumed a `*` in state 6, while state 8 happens after consuming a `+` in state 5.

Suppose we use `1 + 2 * 3` as the input and that the parser has arrived at `1 + 2 . * 3` in state 8 (coming from state 5).
It may either _shift_ to consume `*` and go to state 6 or _reduce_ to accept that it has finished consuming the addition.

In other words, if it shifts, it will parse `1 + (2 * 3)`, giving priority to `*`.
If it reduces, it will parse `(1 + 2) * 3`, giving priority to `+`.
So if we want `*` to bind tighter than `+`, we want to reduce.

Likewise, what if we have `1 + 2 + 3` as the input?
Supposing that we stopped at `1 + 2 . + 3`, we enter state 8 again (coming from state 5).
It may either be parsed as `(1 + 2) + 3` or `1 + (2 + 3)`.
If we shift, we make `+` right-associative, resulting in `1 + (2 + 3)`.
If we reduce, we make `+` left-associative, resulting in `(1 + 2) + 3`.

It means that an LR parser can use shift/reduce conflicts to either shift or reduce, allowing the programmer to specify precedences and associativities of operators.

##### Dangling else

The second well-known case is the dangling-else problem, which is a conflict that arises when a language supports both if-then and if-then-else expressions (like we do in MiniML).

It rises from the fact that the expression `if a then if b then x else y` is ambiguous and may be parsed either as `if a then (if b then x else y)` or `if a then (if b then x) else y`.

Typically, the first interpretation is desirable.
To illustrate, let's see once again the parsing states:

```
exp -> if exp then exp . else exp
exp -> if exp then exp .
```

If we reduced, we would accept the input and get `if a then (if b then x) else b`. So we want to shift instead to continue parsing and get `if a then (if b then x else y)`.

If you think about it, you might notice that this is the same as the operator precedence problem.
So it can be solved similarly, but it's nice to have this extra example in mind.

#### Reduce/reduce conflicts

The second type of conflict is a reduce/reduce conflict, meaning that the parser can reduce an expression using different production rules.
This conflict is more severe than a shift/reduce conflict and is not often as trivially resolvable.
If you ever get it, you should refactor your grammar to eliminate it.

### Finding and solving conflicts in Happy

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

At the top of the file, it will list conflicts.

In my `Parser.info`, I see the following:

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
Remember that shifting makes the operator right-associative, while reducing makes it left-associative.
In this case, we would like to make this token right-associative, so we have two ways of achieving this.

1. Use the `%shift` directive.

Happy allows us to indicate that a production should be shifted by writing `%shift` just before its semantic action.
Thus, our example could be modified like so:

```diff
-  | type '->' type { TArrow (info $1 <-> info $3) $1 $3 }
+  | type '->' type %shift { TArrow (info $1 <-> info $3) $1 $3 }
```

2. Indicate the precedence with `%left`, `%right`, or `%nonassoc`.

Alternatively, at the top part of your grammar, somewhere before `%%`, you may place the following directive:

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

\* Instead of shifting or reducing, they will cause a parse error.

There will be a few more precedences that we will indicate later, but for now, let's continue with what we have.

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

So we have succesfully parsed the example as `int -> ([int] -> ())`.

The complete code for this section may be found [here](https://gist.github.com/heitor-lassarote/d9d6cd856584ab60cf7b3da4932dfc01).

### Parsing expressions

Finally, let's move on to the last part of the grammar we want to parse: expressions.

For now, we can parse three simple expression types: integers, variables, and strings.
Expressions are often more complicated than this – they may have function applications, list literals, local variable declarations, conditionals, etc.

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

A few words about each production, together with an example:

* `EInt`: An integer. `42`.
* `EVar`: A variable. `my_variable`.
* `EString`: A string. `"this is a string"`.
* `EUnit`: The unit literal, with type `()`. `()`.
* `EList`: A list literal. `[1, my_var, if true then 1 else 0`].
* `EPar`: An expression within parentheses. Like `TPar`, it could have been omitted as well. `(1 + foo bar)`.
* `EApp`: The application of an expression in a function. `my_func 1 "foo"`.
* `EIfThen`: A conditional producing a unit. `if true then print "Hello"`.
* `EIfThenElse`: A conditional. `if true then 1 else 2`.
* `ENeg`: The negation of an expression. `-(x + 5)`.
* `EBinOp`: The application of a binary operator to two expressions. `2 * 3 - 5`.
* `EOp`: A binary operator without operands. `(+) x 1`.
* `ELetIn`: Locally declares a variable. `let x = 5 in my_function x`.

We've previously defined a few primitives, namely `integer`, `name`, and `string`.
We may now add a few more simple productions.
Just so we're on the same page, I'll repeat the productions of `exp` that we already have.

```happy
exp :: { Exp L.Range }
  : integer                  { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name                     { EVar (info $1) $1 }
  | string                   { unTok $1 (\range (L.String string) -> EString range string) }
  | '(' ')'                  { EUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '[' sepBy(exp, ',') ']'  { EList (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '(' exp ')'              { EPar (L.rtRange $1 <-> L.rtRange $3) $2 }
```

The only thing new here is the `sepBy` function, which separates various productions (`exp`) by a separator (`,`), just like a list in Haskell.
The definition of `sepBy` is provided below.

```happy
sepBy_rev(p, sep)
  :                         { [] }
  | sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }
```

Parsing such "atoms" is simple enough, but now we will find difficulties in parsing the other types of expressions since they will have more complicated interactions.

#### Parsing function application

Let's begin with `EApp`. You might want to add a new production defining it like so:

```happy
  | exp exp                  { EApp (info $1 <-> info $2) $1 $2 }
```

But we'll immediately see that it will produce five shift/reduce conflicts.
The ambiguity comes from the fact that `a b c` could be parsed either as `(a b) c` or `a (b c)`.
If you analyze the output of `happy -i src/Parser.y`, you should see that it can either shift to another token or reduce it to a function application.

Remember, shifting asks Happy to continue parsing, making the output right-associative, while reducing asks Happy to accept what we have so far, making the output left-associative.

Since shifting is the default action, we'll get an incorrect output with `let x = a b c`:

```haskell
Right
  [ Dec _ (Name _ "x") [] Nothing (EApp _ (EVar _ (Name _ "a")) (EApp _ (EVar _ (Name _ "b")) (EVar _ (Name _ "c"))))
  ]
```

Unfortunately, Happy doesn't have a `%reduce` directive, only a `%shift` one.

One feature supported by Happy is the `%prec` directive, which allows creating a placeholder precedence such as `%left APP`, and then using it like this:  `| exp exp %prec APP` (see [2.3.2](https://monlih.github.io/happy-docs/#_sec_precedences) in the Happy User Guide).
It might cause shift/reduce conflicts later on.
That could be fixed by listing all terminals in the precedence table, though.
Sometimes it might be less annoying than refactoring the grammar, but it's not our case here.

Instead of messing with precedences, we will make an observation that will allow us to refactor our grammar in an intelligent way to resolve this ambiguity: the right side of a function application will always be an "atom".

To be more precise, ambiguities, in general, should appear every time we have a production such as `A B C ... exp` or `exp ... X Y Z`.
Had we placed another token to the left or the right of `exp exp`, such as `exp exp in`, the ambiguity would be eliminated (in this specific case).

The solution is to extract all atoms into one production and use `atom` on the right side of the application:

```happy
exp :: { Exp L.Range }
  : exp atom                 { EApp (info $1 <-> info $2) $1 $2 }
  | atom                     { $1 }

atom :: { Exp L.Range }
  : integer                  { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | name                     { EVar (info $1) $1 }
  | string                   { unTok $1 (\range (L.String string) -> EString range string) }
  | '(' ')'                  { EUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '[' sepBy(exp, ',') ']'  { EList (L.rtRange $1 <-> L.rtRange $3) $2 }
  | '(' exp ')'              { EPar (L.rtRange $1 <-> L.rtRange $3) $2 }
```

This will cause Happy to build a chain like `(((atom atom) atom) atom) atom` when parsing.
Note that even though the left side was stated to be an `exp`, we must eventually reach an `atom` to terminate parsing.

With this, function application should now be left-associative.

#### Parsing conditional expressions

Let's look at another case where we get ambiguities: conditional expressions.

Adding these rules to `exp` will make Happy indicate 11 shift/reduce conflicts:

```happy
  | if exp then exp          { EIfThen (L.rtRange $1 <-> info $4) $2 $4 }
  | if exp then exp else exp { EIfThenElse (L.rtRange $1 <-> info $6) $2 $4 $6 }
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

Shifting will cause the first parse to be accepted, while reducing will cause the second parse to be accepted.
Remember, intuitively, shifting can be approximated as "continue parsing and see what we get" while reducing as "accept what we have so far".
This is why reducing inserts parenthesis right after `f` is parsed.

For now, just like we split `exp` into `exp` and `atom` in the past, we can further split it:

```happy
exp :: { Exp L.Range }
  : expapp                   { $1 }
  | expcond                  { $1 }

expapp :: { Exp L.Range }
  : expapp atom              { EApp (info $1 <-> info $2) $1 $2 }
  | atom                     { $1 }

expcond :: { Exp L.Range }
  : if exp then exp          { EIfThen (L.rtRange $1 <-> info $4) $2 $4 }
  | if exp then exp else exp { EIfThenElse (L.rtRange $1 <-> info $6) $2 $4 $6 }
```

We use `expapp` to represent function applications, which will create a chain of `atom`s that represents the application.
Likewise, `expcond` represents the conditional expressions.
Since the left-hand side of an application can't be a non-atom anymore, the conflict is eliminated.
To write an `if` expression in an application, you will now need to surround it with parentheses.

It's important to note that extracting the conditional expressions from `exp` into their own `expcond` production is unnecessary. Still, it makes the grammar a bit more organized, in my opinion. :)

Be careful not to duplicate `atom` at the end of both `expapp` and `expcond`. If you do, you will get reduce/reduce conflicts since Happy won't be able to figure out if an `atom` should be reduced using `expapp` or `expcond`.

With this, we now only have one shift/reduce conflict, which is the second ambiguity that needs to be resolved.
Recall that we've discussed before in this article that there is a conflict when parsing `if exp then exp` and `if exp then exp else exp`, which is the dangling-else problem.

In this case, we know that for an expression such as `if a then if b then c else d`, shifting will result in the correct answer of `if a then (if b then c else d)` while reducing will result in `if a then (if b then c) else d`.
Check the information file, where it will indicate that the conflict will either reduce with `if exp then exp` or shift to a new state to parse the `else exp` case.

The solution here is simple enough, just use the `%shift` directive:

```diff
-  : if exp then exp          { EIfThen (L.rtRange $1 <-> info $4) $2 $4 }
+  : if exp then exp %shift   { EIfThen (L.rtRange $1 <-> info $4) $2 $4 }
   | if exp then exp else exp { EIfThenElse (L.rtRange $1 <-> info $6) $2 $4 $6 }
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

Now, let's make it so we're able to negate expressions.

This case is pretty simple, just stick this definition in `exp`:

```happy
  | '-' exp                  { ENeg (L.rtRange $1 <-> info $2) $2 }
```

This will also allow us to parse interesting cases, such as `-if True then 1 else 2` or `-succ 2`.
Try it in GHCi.
Both cases are valid Haskell code, and we also have made the first valid MiniML code now.

#### Parsing binary operators

Now let's move on to parsing binary operators, where Happy will make us unhappy again.

First, add new productions to parse each existing operator to `exp`:

```happy
  -- Arithmetic operators
  | exp '+'  exp             { EBinOp (info $1 <-> info $3) $1 (Plus (L.rtRange $2)) $3 }
  | exp '-'  exp             { EBinOp (info $1 <-> info $3) $1 (Minus (L.rtRange $2)) $3 }
  | exp '*'  exp             { EBinOp (info $1 <-> info $3) $1 (Times (L.rtRange $2)) $3 }
  | exp '/'  exp             { EBinOp (info $1 <-> info $3) $1 (Divide (L.rtRange $2)) $3 }
  -- Comparison operators
  | exp '='  exp             { EBinOp (info $1 <-> info $3) $1 (Eq (L.rtRange $2)) $3 }
  | exp '<>' exp             { EBinOp (info $1 <-> info $3) $1 (Neq (L.rtRange $2)) $3 }
  | exp '<'  exp             { EBinOp (info $1 <-> info $3) $1 (Lt (L.rtRange $2)) $3 }
  | exp '<=' exp             { EBinOp (info $1 <-> info $3) $1 (Le (L.rtRange $2)) $3 }
  | exp '>'  exp             { EBinOp (info $1 <-> info $3) $1 (Gt (L.rtRange $2)) $3 }
  | exp '>=' exp             { EBinOp (info $1 <-> info $3) $1 (Ge (L.rtRange $2)) $3 }
  -- Logical operators
  | exp '&'  exp             { EBinOp (info $1 <-> info $3) $1 (And (L.rtRange $2)) $3 }
  | exp '|'  exp             { EBinOp (info $1 <-> info $3) $1 (Or (L.rtRange $2)) $3 }
```

**NB**: Do NOT extract these operators to a new production and change it to `exp operator exp` because of a limitation in Happy. This will cause conflicts that will be pretty difficult to resolve.
You could work around it by using `%prec`, but you will have to list all lookahead terminals in the operator precedence table.

Remember that we previously defined the precedences and associativities of each operator. Otherwise, Happy would generate 168 shift/reduce conflicts. I will copy and paste them here yet again in case you've missed them:

```happy
%right '->'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
```

With this, we now have 12 shift/reduce conflicts.

You could observe that the left-hand side of an operator is always an `atom` and change it accordingly, but this will be no good.
Even though it would remove all of the shift/reduce conflicts, it would also have the side effect of the tree always being right-biased.
You could live with that and properly balance the tree afterward (see the "rotation" method for [Dynamic infix operators with Alex and Happy](https://gist.github.com/heitor-lassarote/b20d6da0a9042d31e439befb8c236a4e) at the end of the article on how to do it). Still, in this case, it's possible to resolve this only by tweaking the grammar, so let's do it.

The reason we need to keep `exp operator exp` and not `atom operator exp` is because the first alternative can have ambiguities, such as parsing `1 + 2 + 3` as either `(1 + 2) + 3` or `1 + (2 + 3)`. The `%left`, `%nonassoc`, and `%right` directives will properly reduce, error, or shift based on the conflicts.
The second alternative would have no ambiguities and always build a right-biased tree with a chain of `atom operator (atom operator atom)`, similar to function application.

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

I don't want to fiddle too much with the grammar or the LR parsing table. So for now, let's add a precedence to `else`:

```diff
+%right else
 %right '->'
 %left '|'
 %left '&'
```

This will cause the `else` keyword to extend as far to the right as possible, as it will have precedence over everything else.

#### Parsing an operator atom

This case is trivial, albeit annoying. Just shove these up in `atom`:

```happy
  -- Arithmetic operators
  | '(' '+' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Plus (L.rtRange $2)) }
  | '(' '-' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Minus (L.rtRange $2)) }
  | '(' '*' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Times (L.rtRange $2)) }
  | '(' '/' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Divide (L.rtRange $2)) }
  -- Comparison operators
  | '(' '=' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Eq (L.rtRange $2)) }
  | '(' '<>' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Neq (L.rtRange $2)) }
  | '(' '<' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Lt (L.rtRange $2)) }
  | '(' '<=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Le (L.rtRange $2)) }
  | '(' '>' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Gt (L.rtRange $2)) }
  | '(' '>=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Ge (L.rtRange $2)) }
  -- Logical operators
  | '(' '&' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (And (L.rtRange $2)) }
  | '(' '|' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Or (L.rtRange $2)) }
```

#### Parsing let-in

Finally, let's see how we can do `let...in...`, the final expression in our language.

Let's add a new case to `exp` and see what happens:

```happy
  | dec in exp               { ELetIn (info $1 <-> info $3) $1 $3 }
```

12 shift/reduce conflicts. Not cool. However, by looking at `Parser.info`, we can see this is a case that we are already familiar with.
It is similar to how conditional expressions interacted with operators, although now with `let...in...` instead of `if...then...else...`.

To illustrate, should `let a = b in a + b` be parsed as `(let a = b in a) + b` or `let a = b in (a + b)`?
The second case is desirable, which is shifting when we reach `in`.

The solution is simply to add a precedence for `in`:

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

Or, after simplifying a bit:

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
 %lexer { lexer } { L.RangedToken L.EOF _ }
+%expect 0
```

This tells Happy that it should expect exactly zero shift/reduce conflicts.
Happy will always expect no reduce/reduce conflicts when this directive is present, no matter the provided value.

You can find the complete grammar [here](https://gist.github.com/heitor-lassarote/5f24f40c8625f25c6108ee59dc10e6d5).

## Exercises

1. Change your lexer and parser to accept fractional numbers.
  Besides accepting numbers such as `3.14`, it should also accept exponents, like `12.5e-4`.

     <details>
     <summary><b>Hint</b></summary>
     Use the <a href=https://hackage.haskell.org/package/scientific-0.3.7.0><code>scientific</code></a> package to read and store the number in your list       of tokens and then in your abstract syntax tree. You may replace <code>Integer</code> with a new <code>Number</code> type if you prefer.
     </details>

2. Support accessing list positions in your lexer and parser. For example, accessing the first element of a list would look like `my_list.(0)`. Note that it may also nest, e.g., `[[1, 2], [3]].(if foo then 0 else 1).(0)`.

3. Change the grammar to support patterns in declarations. Patterns are mainly similar to a subset of expressions, like a few of the atoms, such as numbers, parentheses (although only accepting patterns inside), lists, and strings.

     <details>
     <summary><b>Hint</b></summary>
     Rename <code>Argument</code> to <code>Pattern</code> and work from there. The case for <code>'(' pattern typeAnnotation ')'</code> may itself be an extra "annotation pattern".
     </details>

Bonus: Support `match...with...` in your grammar.

You can find the solutions to exercises 1, 2, and 3 [here](https://gist.github.com/heitor-lassarote/9cc4203990352f5df5f6b84da9291df9) (includes also the lexer exercises).

## Conclusion

In this tutorial, we successfully integrated Alex with Happy and made a parser that will parse the incoming token stream from Alex into an abstract syntax tree.

After reading the series, we hope that you can now use these tools to make valuable things while understanding how to overcome the various challenges that arise when creating grammars.

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

Found a typo in the article or a bug in the code? You can open an issue or a pull request in [our GitHub](https://github.com/serokell/blog-posts).

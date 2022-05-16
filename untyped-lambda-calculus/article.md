# A Brief Look at Untyped Lambda Calculus

When talking about Haskell, the term "lambda calculus" often crops up.
It's a theoretical framework used to define the meaning of computation in many functional languages, such as Haskell, Agda, Idris, etc.
Understanding lambda calculus can be very helpful when talking about programs written in these languages.

Perhaps surprisingly, understanding lambda calculus can also be helpful in understanding the C++ template metalanguage since it's a functional language in its own right (although it's not, strictly speaking, based on lambda calculus).
It's also used extensively in programming language theory research and functional language design.

There are two broad kinds of lambda calculus: _untyped_ and _typed_.
The untyped lambda calculus, discussed in this article, expresses an unrestricted computation.
While it doesn't find much use outside of computability theory, it's necessary to understand it to meaningfully discuss the typed varieties.

## A bit of history

> 1936 - Alan Turing invents every programming language that will ever be but is shanghaied by British Intelligence to be 007 before he can patent them.
>
> 1936 - Alonzo Church also invents every language that will ever be but does it better.
> His lambda calculus is ignored because it is insufficiently C-like.
> This criticism occurs in spite of the fact that C has not yet been invented.
>
> -- James Iry, [A Brief, Incomplete, and Mostly Wrong History of Programming Languages][brief-incomplete-history]

[brief-incomplete-history]: http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html

Lambda calculus, initially envisioned as a formal logic system, was developed by [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church) around the 1930s to explore the foundations of mathematics.

The initial formulation had a logical inconsistency known as the [Kleene–Rosser paradox] (Cantini, 2007).
To sidestep this issue, Church isolated the part of lambda calculus relevant only to computation in 1936.
This isolate is now known as the untyped lambda calculus.

Later, in 1940, a typed version of lambda calculus based on Russel's type theory was introduced.
It has weaker expressive power, but it's logically consistent.

[Kleene–Rosser paradox]: https://en.wikipedia.org/wiki/Kleene%E2%80%93Rosser_paradox

In the mid-1960s, Peter Landin showed that lambda calculus models arbitrarily complex programming languages.
Arguably, this insight kickstarted the research on functional programming languages.

## The pure untyped lambda calculus

The simplest, smallest type of lambda calculus is the pure untyped lambda calculus, so we'll start with it.

Like any untyped lambda calculus, it is first and foremost a consistent system for rewriting expressions.
And it's called pure because it doesn't have anything beyond what's strictly necessary – it only contains functions and variables.

We can formally define the pure untyped lambda calculus in terms of _abstractions_ and _applications_.

### Abstractions

An abstraction, or a functional abstraction, is a parametric expression, that is to say, a function.
Lambda calculus only defines univariate (single-variable) functions, but we can easily extend it to include multivariate functions too, as we'll see later.

We introduce new abstractions using the $\lambda$ symbol.
An abstraction consists of a _head_ and a _body_, separated by the dot ($.$) symbol.
The head contains the $\lambda$ symbol and the argument name.
The body is an arbitrary expression.
For example, $\lambda x. x$ is an abstraction.

Lambda calculus derives its name from the $\lambda$ symbol used in this notation.
Church has stated a couple times the symbol was chosen arbitrarily.
However, there's some evidence (Cardone, 2006) that the notation was derived from $\hat x$ used for class abstraction by Whitehead and Russell in Principia Mathematica, morphing first into $\Lambda x$ and later into $\lambda x$.

Variables in the body of an abstraction that are parameters of said abstraction are called _bound variables_.
Other variables are called _free variables_.

For example, consider the expression:

$\lambda y. (\lambda z. x\;y\;z)\;z.$

Variables $y,$ $z$ in the body of the inner abstraction $(x\;y\;z)$ are bound in this expression.
Variable $x$ is free everywhere.
The last variable $z$ in the outer abstraction $\lambda y. \ldots\;z$ is also free, because no abstraction binds (i.e. introduces) it before it is used.
All bindings are local.

An expression without free variables is called a _closed term_ or a _combinator_.

For example, the expression $\lambda x.\lambda y.\lambda z. x\;y\;z.$ is a combinator because $x$, $y$, and $z$ are bound in one of the outer (relative to their use) abstractions.
However, the sub-expression $\lambda y.\lambda z. x\;y\;z$ _by itself_ is not.

The choice of bound variable names is arbitrary.
Expressions that differ only in the names of bound variables are called _alpha-equivalent_.
For example, $\lambda x. x$ and $\lambda y. y$ are alpha-equivalent, but $\lambda x. y$ and $\lambda y. z$ are not.

And, as usual in abstract mathematics, we can assign a name to an expression.
Here we'll be using the equals sign ($=$) for that.
For example: $id = \lambda x. x.$

### Applications

Application is the operation of substituting a specific value for the parameter in the abstraction.
It's the only operation defined in the pure untyped lambda calculus.

In the standard notation, application is denoted simply by whitespace.
For example, $f\; x$ is the application of $f$ to $x$.
Since all abstractions are univariate by definition, there's no special syntax for multivariate application.

You might notice that using whitespace can lead to ambiguity.
We'll use parentheses where necessary to avoid it.
Additionally, we will assume that application is left-associative and binds more tightly than abstractions.
Hence, $f\;a\;b\;c$ is $((f\;a)\;b)\;c$, i.e. $f$ is applied to $a$, then the result is applied to $b$, then to $c$.
Since application binds more tightly, we don't need parentheses around the body of an abstraction.
However, we might need parenthesis around the whole abstraction itself.
We will also eschew parentheses for nested abstractions, e.g. $\lambda x. \lambda y. x\;y$ is $\lambda x. (\lambda y. (x\;y)).$

## Defining computation

A step of computation in lambda calculus is called beta-reduction.
It's a single application of one expression to another.
Formally, beta-reduction uses the following rule.

<hr>

**Beta-reduction**

Given an expression of the form $(\lambda x. t_1)\; t_2$ (i.e. an application of some abstraction to some expression $t_2$):

1. Rename the bound variables in $t_1$ and $t_2$ to avoid ambiguity.
    The new expressions, $t_1'$ and $t_2',$ must be alpha-equivalent to $t_1$ and $t_2,$ respectively.
2. Replace all the instances of the bound variable $x$ in $t_1'$ with $t_2'.$
    That is the result of the computation.

<hr>

Any expression where beta-reduction can be applied, we'll call a redex (short for reducible expression).

For example, the expression $(\lambda x. x)\;y$ is a redex, and one step of beta-reduction transforms it to just $y$.

According to the rule, we rename bound variables if there might be ambiguity.
For example, $(\lambda x. \lambda y. x\;y)\;(\lambda x. x\;y)$ is a redex.
However, the $y$ identifier is ambiguous.
In the left term, it refers to the bound variable; in the right one, it refers to a free variable.

To avoid ambiguity, we can rename $y$ in the first expression to, say, $z$:

$(\lambda x. \lambda z. x\;z)\;(\lambda x. x\;y).$

Then we can do the rest of the steps with no fear of confusion:

$\lambda z. (\lambda x. x\;y)\;z.$

In the last example, you might wonder if we should also apply beta-reduction inside the abstraction body.
This brings us to the discussion of evaluation strategies.

### Evaluation strategies

An evaluation strategy is a rule that defines which redexes are reduced and in what order.

The simplest approach is to reduce redexes in any arbitrary order until there's nothing left.
This approach is called _full beta-reduction._
But it has some issues.
For instance, you might get different intermediary results depending on the reduction order.

So it's better to reduce in a specific order.
There are multiple ways to approach this.

Our decision points are:
* to start with the left or right side of the expression;
* to start with the outermost or the innermost redex;
* to reduce redexes inside abstraction bodies or not.

#### Direction of reduction

We can reduce an expression left-to-right or right-to-left.
This decision is easy: going left-to-right is strictly more powerful because the process terminates on all terms for which going right-to-left does and then some.

#### Reducing outside-in or inside-out

Second, we can choose to first reduce the outermost or the innermost redex.
The outermost redex is a redex not contained within any other redex, and the innermost redex is a redex that does not contain any other redexes.

To see what we mean, consider the following expression:

$(\lambda x. x)\;((\lambda y. y)\;z).$

This expression contains two redexes, itself and $(\lambda y. y)\;z.$
The former is not contained within any other redex, so it is the outermost of the two.
The sub-expression does not contain any other redex, so is the innermost.

Reducing the leftmost outermost redex first is called the _normal order strategy_, and reducing the leftmost innermost first is called the _applicative order strategy_.

In other words, the applicative order strategy first reduces all of the arguments (from left to right) and then applies the abstraction, while normal order does the inverse.

Expressions that are not reducible using the chosen evaluation strategy are said to be in the _normal form_.

If a given term has a normal form under both of these strategies, it is the same for both.
But the applicative order strategy is strictly weaker than the normal order strategy: some terms that normalize under normal order don't normalize under applicative order.

#### Reducing abstraction bodies or not

The two strategies above are free to go inside abstractions to find redexes.
For this reason, we call them _strong_.

But there are also strategies that can't go inside abstraction bodies, which we call _weak_.
When we're talking about programming languages, we can't necessarily inspect function bodies, so the weak strategies are considered more practical there.

The weak strategy corresponding to the normal order is called _call-by-name_, and the one corresponding to the applicative order is _call-by-value_.

Call-by-name corresponds to lazy evaluation.
In lazy evaluation, some arguments are potentially never evaluated at all, but some computations may be duplicated.
Very few programming languages implement lazy evaluation this way (the only real example that comes to mind is Algol 60).

A more practical variation of call-by-name is _call-by-need_, which is, at least in theory, used in Haskell.
Call-by-need behaves exactly like call-by-name, except that reduction steps that would duplicate computations, don't.
It does that by sharing the computation corresponding to an argument everywhere it appears, so any shared computation is performed at most once.

Call-by-value, on the other hand, corresponds to eager evaluation.
In eager evaluation, arguments are always evaluated before the function call.
Most programming languages follow this strategy by default.

Similar to how applicative order strategy is strictly less powerful than normal order strategy, call-by-value is strictly less powerful than call-by-name.

To sum up, here's a table with the strategies mentioned and their characteristics:

| Name                   | Outermost- or innermost- redex first? | Strong or weak? |
|------------------------|---------------------------------------|-----------------|
| Normal order           | Outermost                             | Strong          |
| Applicative order      | Innermost                             | Strong          |
| Call-by-name (lazy)    | Outermost                             | Weak            |
| Call-by-value (eager)  | Innermost                             | Weak            |

### Eta-reduction

Speaking of reductions, let's also briefly mention _eta_-reduction ($\eta$-reduction).

The rule of eta-reduction says that $\lambda x. f\;x = f$ if $x$ does not appear free in $f$.
It boils down to the statement that two functions are the same if and only if they give the same result for all possible arguments.

Eta-reduction is connected to the point-free style of functional programming.
It enables us to define functions without explicitly listing all the arguments, e.g. we can shorten `g x y z = f (h x) y z` to simply `g = f . h`.

## Programming in pure untyped lambda calculus

> When a pattern shows itself in tiles or on paper or in your mind and says, 'This is the mode of my repetition; in this manner I extend myself to infinity', it has already done so, it has already been infinite from the very first moment of its being; the potentiality and the actuality are one thing.
> If two and two can be four then they actually are four, you can only perceive it, you have no part in making it happen by writing it down in numbers or telling it out in pebbles.
>
> -- Russell Hoban, Pilgermann, Pan, London, 1984.

Now that we have defined the syntax and semantics of lambda calculus, we can talk about expressing various computations in the language.
It is not that different from using any other programming language.

We run into a small issue, however.
We don't have much to work with yet.
There are no booleans, numbers, tuples, or multivariate functions.

No worries, though: we can represent all of that using only abstractions and applications!

### Multivariate functions

Any multivariate function is the same as multiple nested functions returning other functions.
If you're familiar with functional programming, you might know such functions are called curried functions, named after [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry).
In pure lambda calculus, all functions are curried.

To simplify the notation, we will now introduce a shorthand syntax.
A multivariate abstraction of $n$ parameters, declared as $\lambda x_1\;x_2\;\ldots\;x_n. t,$ is the same as $n$ nested abstractions $\lambda x_1. \lambda x_2. \ldots \lambda x_n. t.$

Multivariate abstractions, naturally, can be partially applied, e.g. $(\lambda x\; y\; z. (x\;y)\;(x\;z))\;a\;b$ reduces to $\lambda z. (a\;b)\;(a\;z).$

### Church booleans

Let's introduce booleans now.
We'll do it the same way as Church did in his original paper.
However, that's not the only way.
Arguably, there are infinitely many ways to encode values as functions.

So, let's declare two combinators:
$tru = \lambda x\; y. x,$ and $fls = \lambda x\;y. y.$
We're calling these $tru$ and $fls$ to disambiguate them from actual boolean values $true$ and $false$ that aren't actually abstractions.
$true$ and $false$ are not a part of the pure lambda calculus, but we can introduce them as terms in an impure lambda calculus.

Now, the structure of these definitions is rather curious.
$tru$ is a two-argument function that returns its first argument and ignores the second, while $fls$ returns the second and ignores the first.
If you squint a bit, you might see that this encodes a branching computation.

Indeed, we can define a combinator that would behave exactly like the `if ... then ... else ...` construct: $ifThenElse = \lambda c\;t\;f. c\;t\;f.$
Since Church boolean values themselves encode the behavior, this function doesn't do much.
But it's nice to see we can have familiar branching constructs almost right away.

Now that we have booleans, we can define operations on booleans, i.e. a boolean algebra.
We can define all the usual boolean functions in terms of Church booleans.

For example $and = \lambda x\;y. x\;y\;fls.$
The $and$ function must return $tru$ if and only if both its arguments are $tru.$
Let's make sure it does what we expect it to.

$and\;tru\;tru = (\lambda x\;y. x\;y\;fls)\;(\lambda x\; y. x)\;(\lambda x\; y. x).$

We will rename the bound variables to avoid ambiguity:

$(\lambda x\;y. x\;y\;fls)\;(\lambda a\; b. a)\;(\lambda c\; d. c).$

Now we can reduce the expression.
Let's use the call-by-value strategy here.
The arrow $\to$ represents one evaluation step.

$(\lambda x\;y. x\;y\;fls)\;(\lambda a\; b. a)\;(\lambda c\; d. c)$
$\to (\lambda y. (\lambda a\; b. a)\;y\;fls)\;(\lambda c\; d. c)$
$\to (\lambda a\; b. a)\;(\lambda c\; d. c)\;fls$
$\to (\lambda b. (\lambda c\; d. c))\;fls$
$\to (\lambda c\; d. c) = tru.$

So far so good.
Now let's consider the case of $and\;fls\;tru:$

$and\;fls\;tru$
$= (\lambda x\;y. x\;y\;fls)\;(\lambda a\; b. b)\;(\lambda c\; d. c)$
$\to (\lambda y. (\lambda a\; b. b)\;y\;fls)\;(\lambda c\; d. c)$
$\to (\lambda a\; b. b)\;(\lambda c\; d. c)\;fls$
$\to (\lambda b. b)\;fls$
$\to fls.$

That also works.

The remaining two cases are left as an exercise for the reader, but it should be rather obvious that the combinator works as intended.
Indeed, since $tru$ essentially chooses its first argument and $fls$ its second, $x\;y\;fls$ essentially means "if $x$ is $tru$, the result is $y$, otherwise $fls$".

Defining other boolean combinators is reasonably straightforward by analogy.

### Church pairs

Since we have booleans now, we can also encode pairs (and, by extension, tuples) as functions that return one value when given $tru$ as an argument, and another when given $fls$.

First of all, let's define a pair constructor combinator:

$pair = \lambda f\;s\;b. b\;f\;s.$

It looks suspiciously similar to our $ifThenElse$ combinator, the only difference being that the condition is now the last argument.

Now we can define two combinators for deconstructing a pair:

$fst = \lambda p. p\;tru,$\
$snd = \lambda p. p\;fls.$

It's reasonably straightforward to show that these definitions work as intended.

Consider, for instance:

$fst\;(pair\;x\;y)$
$= (\lambda p. p\;tru)\;((\lambda f\;s\;b. b\;f\;s)\;x\;y)$
$\Rightarrow (\lambda p. p\;tru)\;(\lambda b. b\;x\;y)$
$\to (\lambda b. b\;x\;y)\;tru$
$\to tru\;x\;y$
$= (\lambda a\; b. a)\;x\;y$
$\Rightarrow x.$

Here, double arrow ($\Rightarrow$) signifies multiple steps of beta-reduction.

### Church naturals

Since we encoded both pairs and booleans, we already can encode naturals simply as sequences of booleans, that is, as binary numbers.
In practice, this turns out to be cumbersome, however.

Instead, we'll define naturals similarly to the [Peano construction](https://wiki.haskell.org/Peano_numbers).
Essentially, we'll be choosing an encoding for zero and then encoding other numbers as some successor function applied to zero.

Church used the following definitions:

$c_0 = \lambda s\;z. z,$\
$c_1 = \lambda s\;z. s\;z,$\
$c_2 = \lambda s\;z. s\;(s\;z),$\
etc.

The beautiful thing about this encoding is that it naturally encodes the concept of counting.
Each Church natural takes a "next" combinator $s$ and a "starting value" $z,$ and then "counts" the corresponding "nexts" from $z.$

Now we can define a successor function that takes a natural and returns the next natural.
Since we defined naturals as functions of two arguments, we expect the successor function to be a function of three arguments (i.e. three nested abstractions).
The body should apply the "next" argument to the natural one more time:

$succ = \lambda n\; s\; z. s\;(n\;s\;z).$

It might be easier to see what's going on if we explicitly note the outer abstraction:

$succ = \lambda n. \lambda s\; z. s\;(n\;s\;z).$

In the same manner, we can define addition and multiplication:

$plus = \lambda m\; n. \lambda s\; z. m\;s\;(n\;s\;z),$\
$mul = \lambda m\; n. \lambda s\; z. m\;(n\;s)\;z.$

The former counts $m$ starting from $n$, and the latter counts $m$ times $n$ starting from $z.$

We can also easily test whether a value corresponds to zero (i.e. $c_0$) or not:

$isZero = \lambda n. n\;(\lambda x. fls)\;tru.$

But we run into a bit of an issue when trying to define subtraction or a predecessor function.
Turns out, it's doable but neither pretty nor efficient.
We'll use pairs of values containing two subsequent naturals.
That way, we can have a counter that's lagging by one.

To simplify the notation, we'll introduce two named helper abstractions:

$zz = pair\;c_0\;c_0$\
$ss = \lambda nn. pair\;(snd\; nn)\;(succ\;(snd\;nn)).$

The first element of the pair here is the predecessor, and the second is the successor.
$zz$ encodes the starting point, and $ss$ is the lagging successor.

Now we can define the predecessor function:

$pred = \lambda n. fst\; (n\; ss\; zz).$

With this definition, $pred\;c_0 = c_0,$ which isn't ideal.
However, we don't have an encoding for negative numbers, so we don't have much of a choice.
If we need to, we can use any other value as the first element of $zz$ to signify failure.

### Recursion

While we're on the topic of signifying failure, there's the curious case of expressions for which beta-reduction never stops.

Such expressions don't have a normal form, and are said to _diverge_.

The simplest diverging expression is the application of the so-called omega combinator to itself:

$\Omega = \lambda x. x\;x.$

Using any typical evaluation strategy, one step of beta-reduction of $\Omega\;\Omega$ gives us $\Omega\;\Omega$ again.

The omega combinator is somewhat useless on its own.
However, we can extend it to be a little more useful.
In particular, we can encode recursion using the fixed point combinator, also known as the Y combinator:

$Y = \lambda f. (\lambda x. f\;(x\;x))\;(\lambda x. f\;(x\;x)).$

A shorter equivalent form is sometimes cited:

$X = \lambda f. \Omega\; (\lambda x. f\;(x\;x)).$

We should note that it's not the only fixed-point combinator.
For instance, another famous one is the Turing's combinator (named after its discoverer):

$\Theta = (\lambda x\;y.y\; (x\; x\; y))\; (\lambda x\;y.y\; (x\; x\; y)).$

The idea here is to give us the ability to express recursion by passing the function as its first argument.
The omega combinator infinitely replicates itself.
The Y combinator exploits the same potentially infinitely replicating structure to encode recursion.

Let's see how it works in practice.
Consider a function that counts down to zero using the $pred$ combinator defined in the previous section, and returns zero once the argument is zero.
To define it, we will add a recursive call as its first argument:

$cdown = \lambda f. \lambda n. ifThenElse\;(isZero\;n)\;c_0\;(f\;(pred\;n)).$

Now we can see how $Y\;cdown$ would work:

$Y\;cdown\;c_2$
$= (\lambda f. (\lambda x. f\;(x\;x))\;(\lambda x. f\;(x\;x)))\;cdown\;c_2$
$\to (\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))\;c_2$
$\to cdown\;((\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x)))\;c_2$
$\Rightarrow (\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))\;(pred\;c_2)$
$\Rightarrow (\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))\;c_1$
$\to cdown\;((\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x)))\;c_1$
$\Rightarrow (\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))\;(pred\;c_1)$
$\Rightarrow (\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))\;c_0$
$\to cdown\;((\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x)))\;c_0$
$\Rightarrow c_0.$

For illustrative purposes, we used full beta-reduction, carefully choosing which terms to reduce first.
We could also have used call-by-name at the cost of intermediate expressions getting rather long-winded.
However, if we tried to apply call-by-value, we would get stuck pretty quickly trying to evaluate $((\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))),$ which diverges under call-by-value.

There are fixed-point combinators that work with call-by-value as well.
One particular example is:

$Y_v = \lambda f. (\lambda x. f\;(\lambda y. x\;x\;y)) \;(\lambda x. f\;(\lambda y. x\;x\;y)),$

It's basically the Y combinator with additional abstractions inserted in the middle (which makes it less eager).

## Conclusions

> Rather like the chassis of a bus, which supports the vehicle but is unseen by its users, versions of lambda calculus or combinatorial logic underpin several important logical systems and programming languages.
> Further, $\lambda$ and CL gain most of their purpose at second hand from such systems, just as an isolated chassis has little purpose in itself.
>
>  -- Felice Cardone, J. Roger Hindley, History of Lambda-calculus and Combinatory Logic

After this, admittedly, a very brief look at the untyped lambda calculus, you hopefully have a bit of a feel for how it works.

To summarize:

- Lambda calculus is a theoretical framework on which functional programming languages are built.
- Lambda calculus has two kinds: untyped and typed.
- The pure untyped lambda calculus only consists of variables, abstractions (i.e. univariate functions), and function applications.
- We can represent any computable function in pure untyped lambda calculus.
- Various encodings can be used to represent booleans, naturals, tuples, etc.

The last three points are somewhat profound if you think about it: we only defined a single operation (two if you count the definition of lambda abstraction), and that got us to anything theoretically computable.
There are other constructions of the universal computer: Turing and Post machines, the Markov algorithm, and general recursive functions.
But, arguably, nothing beats the simplicity of lambda calculus.
One has to appreciate the elegance at least.

The primary point of interest is, of course, not so much the calculus itself but its applications, of which there are many.
The untyped kind, however, doesn't find much use beyond the computability theory.
Still, it's necessary to understand the untyped lambda calculus to meaningfully discuss the typed variety (or, more accurately, varieties).

As for more practical applications, here's a few languages based on lambda calculus:

- Haskell;
- most languages in the ML family, like OCaml and F#;
- most dependently typed languages, like Agda, Coq, F*, Idris;
- Cardano blockchain's Plutus Core.

## Exercises

1. We introduced the implementation of the boolean conjunction for Church-encoded boolean values (the `and` operation).
    Implement disjunction (`or`), inversion (`not`), and exclusive disjunction (`xor`).

2. We did introduce one particular implementation for the successor function for Church-encoded naturals, `succ`.
    It is not the only possible implementation, however.
    Suggest another possible implementation, which is not alpha-equivalent to the one we introduced.

3. Define an exponentiation function for Church-encoded naturals.

4. Define subtraction of Church-encoded naturals using the predecessor function `pred`.
    For the case when the difference is undefined in naturals, i.e. when the subtrahend is bigger than minuend, feel free to return $c_0$ or $\Omega\;\Omega$.

5. Define a function that takes two Church-encoded naturals and returns `tru` if those are equal and `fls` otherwise.

6. Implement a pure untyped lambda calculus interpreter in your favorite programming language.

    <p>
    <details>
    <summary>Tips on handling name collisions</summary>

    One question you might have is how to handle name collisions and encode alpha-equivalence.
    There can be many approaches to this problem, but one of the more common ones is using the de Bruijn encoding.
    The idea is to represent variables without naming them.
    Instead, we can encode variables by their de Bruijn indexes, i.e. a reference to the binding abstraction represented as a natural.
    The natural number is the "nesting level" of the variable relative to its binding abstraction, starting at $0$.
    For example, $\lambda x. x$ can be encoded as $\lambda. 0,$ and $\lambda x\; y. x\;x\;y$ as $\lambda.\lambda. 1\;1\;0.$
    Two terms are alpha-equivalent if and only if their de Bruijn representation is the same.
    We should note that, during beta-reduction, de Bruijn indexes of free variables will change, but they will change predictably.
    Consider, for example, the following term:

    $\lambda. (\lambda. \lambda.\lambda. 1\;(2\;1\;0))\; 0$

    (this is $\lambda x.succ\;x$).
    It is quite apparent that $\lambda. \lambda.\lambda. 1\;(0\;1\;0)$ is not equivalent (injecting variable names back gives $\lambda x. \lambda s.\lambda z. s\;(z\;s\;z),$ when it should be $s\;(x\;s\;z)$).
    But, you might notice that $\lambda. \lambda.\lambda. 1\;(2\;1\;0)$ is (by the rule of eta-reduction).
    This line of reasoning gives us the intuition for the general rule.
    When an expression is substituted, de Bruijn indexes of its free variables must increase by the de Bruijn index of the variable the expression replaces.
    It should also be noted that when the left term contains free variables, those must be reduced by one, because application "strips" one level of $\lambda$, e.g.

    $\lambda. (\lambda. \lambda.\lambda. 1\;(2\;3\;0))\; 0$

    is $\lambda. \lambda.\lambda. 1\;(2\;2\;0),$
    not $\lambda. \lambda.\lambda. 1\;(2\;3\;0)$ (indeed in this example $3$ references a nonexistent abstraction).

    ***

    </details>
    </p>

## References

- (Cardone, 2006): [Cardone, Felice, and J. Roger Hindley. "History of lambda-calculus and combinatory logic." Handbook of the History of Logic 5 (2006): page 7.](https://hope.simons-rock.edu/~pshields/cs/cmpt312/cardone-hindley.pdf)
- (Cantini, 2007): [Cantini, Andrea, and Riccardo Bruni. "Paradoxes and contemporary logic." (2007).](https://plato.stanford.edu/entries/paradoxes-contemporary-logic/#IncoCertFormLogi)

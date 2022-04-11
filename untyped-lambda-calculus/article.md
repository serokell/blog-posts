# Untyped Lambda Calculus

When talking about Haskell, the term "lambda calculus" often crops up.
It is a theoretical framework used to define the meaning of "computation" in many functional languages.
Understanding lambda calculus can be very helpful when talking about Haskell programs.
It is also used extensively in programming language theory research and functional language design.

There are two broad kinds of lambda calculus, _typed_, and _untyped_.
The _untyped_ kind, discussed in this article, expresses an unrestricted computation.
However, as a system of logic, it is self-contradictory.
The _typed_ kind, which we intend to discuss in a follow-up article, is a restricted version.
It doesn't have the logical paradox that plagues the untyped kind and can serve as a logic system.

The list of languages based on lambda calculus includes:

- Haskell
- Most languages in the ML family, like OCaml and F#
- Cardano blockchain's Plutus Core

There's a rumor that John McCarthy based Lisp on lambda calculus, but this seems to be, broadly speaking, false.
The original Lisp used different name resolution semantics, and as much was stated by John himself:

> ... one of the myths concerning LISP that people think up or invent for themselves becomes apparent, and that is that LISP is somehow a realization of the lambda calculus, or that was the intention.
> The truth is that I didn't understand the lambda calculus, really.
>
> -- John McCarthy, Lisp session, History of Programming Languages

Also, while not strictly speaking based on lambda calculus, C++ template metalanguage is a functional language in its own right, so much of what we discuss below happens to be applicable.
In fact, one could either embed lambda calculus into C++ template metalanguage or write a transpiler from a lambda calculus-based language into C++ template metalanguage.
See, for instance, [Translating Lambda Calculus into C++ Templates by Vít Šefl](https://dx.doi.org/10.1007/978-3-030-83978-9_5).

## A bit of history

> 1936 - Alan Turing invents every programming language that will ever be but is shanghaied by British Intelligence to be 007 before he can patent them.
>
> 1936 - Alonzo Church also invents every language that will ever be but does it better.
> His lambda calculus is ignored because it is insufficiently C-like.
> This criticism occurs in spite of the fact that C has not yet been invented.
>
> -- James Iry, [A Brief, Incomplete, and Mostly Wrong History of Programming Languages][brief-incomplete-history]

[brief-incomplete-history]: http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html

Lambda calculus, initially envisioned as a formal logic system, was developed by Alonzo Church around the 1930s to explore the foundations of mathematics.
The initial formulation had a logical inconsistency known as [Kleene–Rosser paradox].
To sidestep this issue, Church isolated the part of lambda calculus relevant only to computation in 1936.
This isolate is now known as the untyped lambda calculus.
Later, In 1940, a typed version of lambda calculus was introduced, which has weaker expressive power but is logically consistent.

[Kleene–Rosser paradox]: https://en.wikipedia.org/wiki/Kleene%E2%80%93Rosser_paradox

The lambda symbol used in the standard notation, from which lambda calculus derives its name, most likely was chosen arbitrarily.
However, there's an apocryphal story the notation was derived from $\hat x$, morphing first into $\Lambda x$, and later into $\lambda x$.

In the mid-1960s, Peter Landin has shown that lambda calculus models arbitrarily complex programming languages.
This insight arguably kickstarted the research on functional programming languages.

## The pure untyped lambda calculus

The untyped lambda calculus is, first and foremost, a consistent equational theory of functions.
Calculus that only contains functions and variables is called the pure untyped lambda calculus.

We can formally define the pure untyped lambda calculus in terms of _abstractions_ and _applications_.

_Abstraction_, or functional abstraction, is a definition of a parametric expression, that is to say, a function.
Lambda calculus only defines univariate functions, but that's easily extended to multivariate, as we'll see later.

We introduce new abstractions using the $\lambda$ symbol.
The abstraction consists of the _head_ and the _body_, separated by the dot ($.$) symbol.
The _head_ contains the $\lambda$ symbol and the parameter name.
The _body_ is an arbitrary expression.
For example, $\lambda x. x$ is an abstraction.
As usual in abstract mathematics, we can assign a name to an expression.
Here we'll be using the equals sign ($=$) for that, for example, $id = \lambda x. x.$

_Application_ is the operation of substituting a specific value for the parameter in the abstraction.
It is the only operation defined in the pure untyped lambda calculus.
In the standard notation, the application is denoted simply by whitespace.
For example, $f\; x$ is the application of $f$ to $x$.
Since all abstractions are univariate by definition, there isn't a special syntax for multivariate application.

You might notice that using whitespace can lead to ambiguity.
We'll use parentheses where necessary to avoid it.
Additionally, we will assume that application is left-associative and binds more tightly than abstractions.
Hence, $f\;a\;b\;c$ is $((f\;a)\;b)\;c$, i.e. $f$ is applied to $a$, then the result is applied to $b$, then to $c$.
Since application binds more tightly, we don't need parentheses around the body of an abstraction.
However, we might need parenthesis around the whole abstraction itself.
We will also eschew parentheses for nested abstractions, e.g. $\lambda x. \lambda y. x\;y$ is $\lambda x. (\lambda y. (x\;y)).$

Variables in the body of an abstraction that are parameters of said abstraction are called _bound variables_.
Other variables are called _free variables_.

An expression without free variables is called a _closed term_ or a _combinator_.

The choice of names of bound variables is arbitrary.
Expressions that differ only in the names of bound variables are called _alpha-equivalent_.
For example, $\lambda x. x$ and $\lambda y. y$ are alpha-equivalent, but $\lambda x. y$ and $\lambda y. z$ are not.

## Defining the computation

The step of the computation is called beta-reduction.
It is a single application of one expression to another.
Formally, beta-reduction uses the following rules.
Given an expression of the form $(\lambda x. t_1)\; t_2$ (i.e. an application of some abstraction to some expression $t_2$):

1. Rename bound variables in $t_1$ and $t_2$ to avoid ambiguity.
    The new expressions, $t_1'$ and $t_2',$ must be alpha-equivalent to $t_1$ and $t_2,$ respectively.
2. Replace all the instances of the bound variable $x$ in $t_1'$ with $t_2'.$
    That is the result of the computation.

Any expression where beta-reduction can be applied, we'll call a redex (**red**ucible **ex**pression).

For example, the expression $(\lambda x. x) y$ is a redex, and one step of beta-reduction transforms it to just $y$.

Per rule 1, we rename bound variables if there might be ambiguity.
For example, $(\lambda x. \lambda y. x\;y)\;(\lambda x. x\;y)$ is a redex.
However, the $y$ identifier is ambiguous.
In the left term, it refers to the bound variable; in the right one, it refers to a free variable.
To avoid ambiguity, we can rename $y$ in the first expression to, say, $z$: $(\lambda x. \lambda z. x\;z)\;(\lambda x. x\;y).$
Then we can do the rest of the steps with no fear of confusion: $\lambda z. (\lambda x. x\;y)\;z.$

In the last example, you might wonder if we should also apply beta-reduction inside the abstraction body.
It brings us to the discussion of evaluation strategies.
Different evaluation strategies correspond to different computation semantics.

A rule that defines which redexes are reduced in what order is called an evaluation strategy.
The most straightforward evaluation strategy is called _full beta-reduction._
It allows us to reduce any redex with no order limitations.
Full beta-reduction does have some issues, however.
For instance, you might get different results depending on the reduction order.
Two common strategies define the order: _normal order strategy_ and _applicative order strategy_.
The former says that we must first reduce the leftmost _outermost_ redex.
In other words, we substitute arguments into the abstraction body before beta-reducing them.
The latter says that we must start with the leftmost _innermost_ redex.
That is, we reduce the arguments before substitution.

Those are the common _strong_ evaluation strategies.
We call them strong because they go inside the abstraction bodies.
The corresponding _weak_ evaluation strategies are called _call-by-name_ and _call-by-value_.
Call-by-name is a weak version of the normal order strategy.
Call-by-value is a little stricter and only allows reduction if the argument is a "value" -- that is, in pure lambda calculus, either a variable or an abstraction.

It's interesting to note that the call-by-value strategy corresponds to eager evaluation.
In eager evaluation, arguments are evaluated before the function call.
Call-by-name, on the other hand, corresponds to lazy evaluation.
In lazy evaluation, the arguments potentially are never evaluated at all.

A practically significant variation on call-by-name is _call-by-need_, which is, at least in theory, used in Haskell.
Call-by-need behaves exactly like call-by-name, except that reduction steps that would duplicate computations, don't.
This is achieved by sharing the computation corresponding to an argument in all places where it is used.

<p>
<details>
<summary>Haskell and call by need</summary>

In theory, Haskell strives to implement call-by-need.
In practice, it is a little more complicated.
The reason is simple -- the optimizer can make non-obvious transformations, which could either remove some duplicated computations or vice versa.

***

</details>
</p>

Expressions that are not reducible using the chosen evaluation strategy are said to be in the _normal form_.

Speaking of reductions, we'll also mention _eta_-reduction ($\eta$-reduction).
The rule of eta-reduction says that $\lambda x. f\;x = f,$ if $x$ does not appear free in $f$.
It boils down to the statement that two functions are the same if and only if they give the same result for all possible arguments.

## Programming in pure untyped lambda calculus

> When a pattern shows itself in tiles or on paper or in your mind and says, 'This is the mode of my repetition; in this manner I extend myself to infinity', it has already done so, it has already been infinite from the very first moment of its being; the potentiality and the actuality are one thing.
> If two and two can be four then they actually are four, you can only perceive it, you have no part in making it happen by writing it down in numbers or telling it out in pebbles.
>
> -- Russell Hoban, Pilgermann, Pan, London, 1984.

Now that we defined both syntax and semantics, we can talk about expressing various computations in the language of lambda calculus.
It is not that different from using any other programming language.

We run into a small issue, however.
We don't have much to work with yet.
We don't have booleans, numbers, tuples, or multivariate functions.
No worries, though!
We can represent all of that using only abstractions and applications!

### Multivariate functions

Any multivariate function is the same as multiple nested functions returning other functions.
If you're familiar with functional programming, you might know such functions are called curried functions, named in honor of Haskell Curry.
In pure lambda calculus, then, all functions are curried.

To simplify the notation, we will now introduce a shorthand syntax.
A multivariate abstraction of $n$ parameters, declared as $\lambda x_1\;x_2\;\ldots\;x_n. t,$ is the same as $n$ nested abstractions $\lambda x_1. \lambda x_2. \ldots \lambda x_n. t.$

Multivariate abstractions, naturally, can be partially applied, e.g. $(\lambda x\; y\; z. (x\;y)\;(x\;z))\;a\;b$ reduces to $\lambda z. (a\;b)\;(a\;z).$

### Church booleans

Let us now introduce booleans.
We will follow Church here.
However, it's not the only way.
Arguably there are infinitely many ways to encode values as functions.

So, let us declare two combinators,
$tru = \lambda x\; y. x,$ and $fls = \lambda x\;y. y.$
We're calling these $tru$ and $fls$ to disambiguate them from atomic boolean values.

Now, the structure of these definitions is rather curious.
$tru$ is a two-argument function that returns its first argument, ignoring the second, and $fls$ returns its second argument.
If you squint a bit, you might see this encodes a branching computation.

Indeed, we can define a combinator that would behave exactly like `if ... then ... else ...` construct: $ifThenElse = \lambda c\;t\;f. c\;t\;f.$
Since Church boolean values themselves encode the behavior, this function doesn't do much.
It's nice to see we can have familiar branching constructs almost right away.

Now that we have booleans, we can define operations on booleans, i.e. a boolean algebra.
We can define all the usual boolean functions in terms of Church booleans.

For example $and = \lambda x\;y. x\;y\;fls.$
The $and$ function must return $tru$ if and only if both its arguments are $tru.$
Let us make sure it does what we expect it to.

$and\;tru\;tru = (\lambda x\;y. x\;y\;fls)\;(\lambda x\; y. x)\;(\lambda x\; y. x)$

We will rename the bound variables to avoid ambiguity:

$(\lambda x\;y. x\;y\;fls)\;(\lambda a\; b. a)\;(\lambda c\; d. c)$

Now we can reduce the expression.
Let's use call by value strategy here.
The arrow $\to$ represents one evaluation step:

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

Since we have booleans now, we can also encode pairs (and by extension, tuples as trees or right combs) as functions, returning one value when given $tru$ as an argument, and another when given $fls$.

First of all, let us define a pair constructor combinator:

$pair = \lambda f\;s\;b. b\;f\;s.$

It looks suspiciously similar to our $ifThenElse$ combinator, the only difference being that condition is now the last argument.

Now we can define two combinators for deconstructing a pair:

$fst = \lambda p. p\;tru,$\
$snd = \lambda p. p\;fls.$

It's reasonably straightforward to show that these definitions work as intended.
Consider, for instance,

$fst\;(pair\;x\;y)$
$= (\lambda p. p\;tru)\;((\lambda f\;s\;b. b\;f\;s)\;x\;y)$
$\Rightarrow (\lambda p. p\;tru)\;(\lambda b. b\;x\;y)$
$\to (\lambda b. b\;x\;y)\;tru$
$\to tru\;x\;y$
$= (\lambda a\; b. a)\;x\;y$
$\Rightarrow x.$

Here, double arrow $\Rightarrow$ signifies more than one step of beta-reduction.

### Church naturals

Since we encoded both pairs and booleans, we already can encode naturals simply as sequences of booleans, that is, as binary numbers.
In practice, this turns out to be cumbersome, however, so instead, we'll define naturals similar to the Peano construction.
Essentially, we'll be choosing an encoding for zero and then encoding other numbers as some successor function applied to zero.

Church used the following definitions:

$c_0 = \lambda s\;z. z\;$\
$c_1 = \lambda s\;z. s\;z,$\
$c_2 = \lambda s\;z. s\;(s\;z),$\
etc.

The beautiful thing about this encoding is that it naturally encodes the concept of "counting".
Each Church natural takes a "next" combinator $s$ and a "starting value" $z,$ and then "counts" the corresponding "nexts" from $z.$

Now we can define a successor function, taking a natural and returning the next natural.
Since we defined naturals as functions of two arguments, we expect the successor function to be a function of three arguments (i.e. three nested abstractions).
The body should apply the "next" argument to the natural one more time:

$succ = \lambda n\; s\; z. s\;(n\;s\;z).$

It might be easier to see what's going on if we explicitly note the outer abstraction:

$succ = \lambda n. \lambda s\; z. s\;(n\;s\;z).$

In the same manner, we can define addition
$$plus = \lambda m\; n. \lambda s\; z. m\;s\;(n\;s\;z),$$
and multiplication
$$mul = \lambda m\; n. \lambda s\; z. m\;(n\;s)\;z.$$
The former counts $m$ starting from $n$, and the latter counts $m$ times $n$ starting from $z.$

We can also easily test whether a value corresponds to zero (i.e. $c_0$) or not:
$$isZero = \lambda n. n\;(\lambda x. fls)\;tru.$$

We run into a bit of an issue when trying to define subtraction or a predecessor function.
Turns out it's doable but neither pretty nor efficient.
We'll use pairs of values containing two subsequent naturals.
That way, we can stagger the counter by one.
To simplify the notation, we'll introduce two named helper abstractions:

$zz = pair\;c_0\;c_0$\
$ss = \lambda nn. pair\;(snd\; nn)\;(succ\;(snd\;nn)).$\

The first element of the pair here is the predecessor, and the second is the successor.
$zz$ encodes the starting point, and $ss$ is the staggered successor.

Now we can define the predecessor function:

$$pred = \lambda n. fst\; (n\; ss\; zz).$$

With this definition, $pred\;c_0 = c_0,$ which isn't ideal.
However, we don't have an encoding for negative numbers, so we don't have much of a choice.
If we need to, we can use any other value as the first element of $zz$ to signify failure.

### Recursion

While we're on the topic of signifying failure, there's a curious case of expressions for which beta-reduction never stops.

Such expressions don't have a normal form, and we call those _diverging_.

The simplest diverging expression is the application of the so-called omega combinator to itself:

$$\Omega = \lambda x. x\;x.$$

Using any typical evaluation strategy, one stem of beta-reduction of $\Omega\;\Omega$ gives us $\Omega\;\Omega$ again.

The omega combinator is somewhat useless on its own.
However, we can extend it to be a little more useful.
In particular, we can encode recursion using the fixed point combinator, also known as $Y$ combinator:

$$Y = \lambda f. (\lambda x. f\;(x\;x))\;(\lambda x. f\;(x\;x)).$$

A shorter equivalent form is sometimes cited:

$$X = \lambda f. \Omega\; (\lambda x. f\;(x\;x)).$$

We should note that it's not the only fixed-point combinator.
For instance, another famous one is

$$\Theta = (\lambda x\;y.y\; (x\; x\; y))\; (\lambda x\;y.y\; (x\; x\; y)).$$

The idea here is to give us the ability to express recursion by passing the function as its first argument.
The omega combinator infinitely replicates itself.
The $Y$ combinator exploits the same potentially infinitely replicating structure to encode recursion.

Let us see how it works in practice.
Consider a function that counts down to zero using the $pred$ combinator, defined in the previous section, and returns zero once the argument is zero.
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
$\Rightarrow c_0$

For illustrative purposes, we used full beta-reduction, carefully choosing which terms to reduce first.
We could also have used call-by-name at the cost of intermediate expressions getting rather long-winded.
However, if we tried to apply call by value, we would get stuck pretty quickly trying to evaluate $((\lambda x. cdown\;(x\;x))\;(\lambda x. cdown\;(x\;x))),$ which under call by value diverges.

There are fixed-point combinators that work with call-by-value as well.
One particular example is

$$Y_v = \lambda f. (\lambda x. f\;(\lambda y. x\;x\;y)) \;(\lambda x. f\;(\lambda y. x\;x\;y)),$$
which is basically the $Y$ combinator with additional abstractions inserted in the middle.

## Conclusions

> Rather like the chassis of a bus, which supports the vehicle but is unseen by its users, versions of lambda calculus or combinatorial logic underpin several important logical systems and programming languages.
> Further, $\lambda$ and CL gain most of their purpose at second hand from such systems, just as an isolated chassis has little purpose in itself.
>
>  -- Felice Cardone, J. Roger Hindley, History of Lambda-calculus and Combinatory Logic

After this, admittedly, a very brief look at the untyped lambda calculus, you hopefully have a bit of a feel for how it works.
The primary point of interest is, of course, not as much in the calculus itself, but in its applications, of which there are many.
However, the untyped kind doesn't find much use beyond the computability theory.
Still, it's necessary to understand the untyped lambda calculus to meaningfully discuss the typed variety (or, more accurately, varieties).

So, let us summarize:

- lambda calculus is a theoretical framework on which functional programming languages are built;
- lambda-calculus has two kinds, untyped and typed;
- the pure untyped lambda calculus only consists of variables, abstractions (i.e. univariate functions), and function applications;
- we can represent any computable function in pure untyped lambda calculus;
- various encodings can be used to represent booleans, naturals, tuples, etc.

The last three points are somewhat profound if you think about it: we only defined a single operation (two if you count the lambda abstraction definition), and that got us to anything theoretically computable.
There are other constructions of the universal computer: Turing and Post machines, the Markov algorithm, and general recursive functions.
But, arguably, nothing beats the simplicity of lambda calculus.
One has to appreciate the elegance at least.

## Exercises

1. We introduced the implementation of the boolean conjunction for Church-encoded boolean values (the `and` operation).
    Implement disjunction (`or`), inversion (`not`), and exclusive disjunction (`xor`).

2. We did introduce one particular implementation for the successor function for Church-encoded naturals, `succ`.
    It is not the only possible implementation, however.
    Suggest another possible implementation, which is not alpha-equivalent to the one we introduced.

3. Define an exponentiation function for Church-encoded naturals.

4. Define subtraction of Church-encoded naturals using the predecessor function `pred`.
    For the case when the difference is undefined in naturals, i.e. when the subtrahend is bigger than minuend, feel free to return $c_0$ or $\Omega\;\Omega$.

5. Define a function taking two Church-encoded naturals and returning `tru` if those are equal and `fls` otherwise.

6. Implement a pure untyped lambda calculus interpreter in your favorite programming language.

    One question you might have is how to handle name collisions and encode alpha-equivalence.
    There can be many approaches to this problem, but one of the more common ones is using the de Bruijn encoding.
    The idea is to represent variables without naming them.
    Instead, we can encode variables by their de Bruijn indexes, i.e. a reference to the binding abstraction represented as a natural.
    The natural number is the "nesting level" of the variable relative to its binding abstraction, starting at $0$.
    For example, $\lambda x. x$ can be encoded as $\lambda. 0,$ and $\lambda x\; y. x\;x\;y$ as $\lambda.\lambda. 1\;1\;0.$
    Two terms are alpha-equivalent if and only if their de Bruijn representation is the same.
    We should note that, during beta-reduction, de Bruijn indexes of free variables will change, but they will change predictably.
    Consider, for example, the following term:
    $$\lambda. (\lambda. \lambda.\lambda. 1\;(2\;1\;0))\; 0$$
    (this is $\lambda x.succ\;x$).
    It is quite apparent that $\lambda. \lambda.\lambda. 1\;(0\;1\;0)$ is not equivalent (injecting variable names back gives $\lambda x. \lambda s.\lambda z. s\;(z\;s\;z),$ when it should be $s\;(x\;s\;z)$).
    But, you might notice that $\lambda. \lambda.\lambda. 1\;(2\;1\;0)$ is (by the rule of eta-reduction).
    This line of reasoning gives us the intuition for the general rule.
    When an expression is substituted, de Bruijn indexes of its free variables must increase by the de Bruijn index of the variable the expression replaces.
    It should also be noted that when the left term contains free variables, those must be reduced by one, because application "strips" one level of $\lambda$, e.g.
    $$\lambda. (\lambda. \lambda.\lambda. 1\;(2\;3\;0))\; 0$$
    is $\lambda. \lambda.\lambda. 1\;(2\;2\;0),$
    not $\lambda. \lambda.\lambda. 1\;(2\;3\;0)$ (indeed in this example $3$ references a nonexistent abstraction).

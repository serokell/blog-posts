# Introduction to Free Monads

If you've been around Haskell circles for a bit, you've probably seen the term "free monads".
If you're relatively new to Haskell, you might feel that monads are hard enough to grasp as is, and now there's these different type of monads to worry about.
Perhaps you've used Haskell for a while but didn't yet get to wrapping your head around free monads.
In any case, the concept is ultimately rather straightforward.
This article aims to introduce free monads and to explain why they are useful.

Basic familiarity with Haskell is assumed for the rest of this article.
Specifically, the `do`-notation, and type classes `Monoid`, `Functor`, and `Monad`.

## Free algebraic structures

> To understand the concept, you should think of "free" as in "free speech," not as in "free beer".
>
> -- Free Software Foundation, What is Free Software?

So, to start with, let us try to explore what "free" in free monads stands for.
This part is a bit theory-heavy, so if it is not your cup of tea, feel free to skim over it.

In abstract algebra, we call something a "free X" when it is in some sense a minimal structure that satisfies conditions for being X.
The exact definition of "minimal" in abstract algebra is a little vague, but the main idea is that "free X" satisfies all laws for X, and does not have any other laws describing it.
That is, in this context, "free" means "unrestricted" -- it is not that we can get a structure "for free"[^for-free], but that no other laws are restricting the structure apart from those absolutely necessary.

[^for-free]: Although in most cases, we actually might, depending on your definition of "for free".

More formally, a free structure over a set $S$ is a set $M,$ together with operations on elements of $M,$ such that:

- There is an embedding $i: S\to M$.
- $M$ only contains elements required to exist by $i$ or the operations on the elements of $M$.
- The only laws that hold for the generated structure are those required to hold.

Let us start with a simple example, specifically, `Monoid`s.
In Haskell, any type `α` in the `Monoid` type class must have two functions defined.
First, to construct a neutral element, we have `mempty :: α`.
Second, to in some sense combine two elements, we have `mappend :: α -> α -> α`.
Additionally, these functions must satisfy three equations, called the monoid laws:

```haskell
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) -- associativity
mempty `mappend` x = x -- left identity
x `mappend` mempty = x -- right identity
```

A free monoid thus has to have these two operations (together with an embedding of some underlying set) and satisfy these laws and only them.
For example, it is implied that commutativity, i.e. the equation ``x `mappend` y = y `mappend` x``, should only hold if `x = y` or `x = mempty`, or `y = mempty`, and not in general.

To give a more concrete example, a list (or, more generally, a sequence) of elements of the set `S` constitutes a free monoid over `S`.
Indeed, if the set $M$ is a set of lists of elements of $S$,

```haskell
type M = [S]
```

then, our embedding can be defined as

```haskell
i :: S -> M
i x = [x]
```

and then `mempty = []` and `mappend = (++)`, all the laws hold, and no other laws are implied.

Similar to monoids, any set- and group-theoretic construction can, in principle, have a free counterpart.
For example, you might have free groups or free rings.

One objection to this somewhat informal introduction might be that the concept of "minimal structure" is not very well-defined.
Category theory offers a way to define a free structure as a left adjunct of some forgetful functor.
However, we will avoid delving too deep into the category theory here.
If you're interested, [Bartosz Milewski's article on adjunctions][adjunctions] delves deeper into the subject.

[adjunctions]: https://bartoszmilewski.com/2016/04/18/adjunctions/

## Monads

> 1990 - A committee formed by Simon Peyton-Jones, Paul Hudak, Philip Wadler, Ashton Kutcher, and People for the Ethical Treatment of Animals creates Haskell, a pure, non-strict, functional language.
> Haskell gets some resistance due to the complexity of using monads to control side effects.
> Wadler tries to appease critics by explaining that "a monad is a monoid in the category of endofunctors, what's the problem?"
>
> -- James Iry, [A Brief, Incomplete, and Mostly Wrong History of Programming Languages][brief-incomplete-history]

[brief-incomplete-history]: http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html

Before we see how this idea applies to monads, it might be helpful to compare monads to monoids first.

Let us first review the definition of the `Monad` type class.
In Haskell, it is defined as[^applicative]

[^applicative]: If you're more familiar with modern Haskell, you might object that actually, the superclass of `Monad` is `Applicative`, but we will gloss over that for the sake of simplicity.

```haskell
class Functor m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

This definition needs to additionally satisfy the monad laws:

```haskell
(m >>= g) >>= h = m >>= (\x -> g x >>= h) -- associativity
return a >>= h = h a -- left identity
m >>= return = m -- right identity
```

If you look at the monad laws and squint a bit (or perhaps a lot), you might notice that these laws seem somewhat similar to the monoid laws.
The names of the laws are the same, at least.
Our choice of the definition makes it a little harder to see the connection, though.
If we instead rewrite these laws using the Kleisli composition

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
```

it becomes much more apparent[^equivalence]:

[^equivalence]: The proof of equivalence is left as an exercise to the reader.

```haskell
(f >=> g) >=> h = f >=> (g >=> h) -- associativity
return >=> f = f -- left identity
f >=> return = f -- right identity
```

That is why a monad is indeed a monoid: it satisfies the same laws, and the "values" are of the type `Functor f => a -> f b`.
Note, though, that this is not the category of endofunctors.
However, but there exists a natural transformation to the category of endofunctors[^endofunctors].

[^endofunctors]: One could alternatively define monads via `join :: m (m a) -> m a` instead of `>>=`, which would make it a little more evident that monads are monoids over endofunctors, but to explain this properly, we would need to use a bit more of category theory jargon.
    There is an excellent [r/math post][endofunctors] on the subject.

    [endofunctors]: https://www.reddit.com/r/math/comments/ap25mr/a_monad_is_a_monoid_in_the_category_of/

We can guess a few things, based on the intuition we gained looking at free monoids.
First, we might note that `return` and `>>=` correspond to `mempty` and `mappend`, respectively.
Second, we can expect that for any `Functor` `f :: Type -> Type`, there is a corresponding free `Monad` `Free f :: Type -> Type`, the same as for free monoids.

## Free monads

> Basically, the idea behind the Free Monad is that all of our computations become a value.
> The idea is that whenever we define our application, it does not really execute itself, but constructs an abstract syntax tree that describes the application that we can run later.
>
> -- Anatolii Kmetiuk, Mastering Functional Programming

Let us first try to construct `Free f` by analogy.
We've seen that lists are free monoids.
The typical definition of a list type in Haskell looks something like this:

```haskell
data List a = Nil | Cons a (List a)
```

Now, `List a` is a type, but `Free f` is a unary type constructor, so we'd need one more type argument.
Doing that directly yields

```haskell
data Free f a = Nil a | Cons f (Free f) a
```

We can't define such a data type, due to `f` not being a plain type and hence can't be an argument of a data constructor, but we're actually very close.

Let us now look at the actual definition of `Free f`:

```haskell
data Free f a = Pure a | Free (f (Free f a))
```

As you can see, it is indeed very similar to a list -- a bit more general, since the functor is arbitrary, which potentially makes it a tree, with branches of type `f` and leaves of type `a`.
Pure values, i.e. leaves, are encoded via the constructor `Pure`, and "actions", i.e. branches -- via the constructor `Free`.

It may be helpful to write out the types of these constructors explicitly:

```haskell
Pure :: a -> Free f a
Free :: f (Free f a) -> Free f a
```

If you squint a bit, you will notice that `Pure` has the same signature as `return` (assuming `Monad (Free f)`).
If you squint some more, you will notice that the signature of `Free` is very similar to the signature of `join`:

```haskell
join :: Monad m => m (m a) -> m a
```

Instead of defining monads in terms of `return` and `>>=`, we can alternatively define them in terms of `map`, `return`, and `join`.
So this hopefully provides some insight into why such a structure works as an encoding for a free monad.
Indeed, if we require that `f` in `Free f` must be a functor, we get `fmap` from the start, and `Pure` and `Free` fill the roles of `return` and `join` respectively.

If `f` is a `Functor`, then `Free f` is also a `Functor`.
Indeed, the instance is very straightforward:

```haskell
instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap f <$> g)
```

Notice that we need `f` to be a `Functor` to recursively descend the `Free` branch.

If `f` is a `Functor`, then `Free f` is also a `Monad`.
The instance is also pretty straightforward:

```haskell
instance Functor f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)
```

Note that the implementation of `>>=` is very similar to that of `fmap`.
If we instead defined the `Monad` instance in terms of `join`, it would look something like this:

```haskell
join (Pure x) = x
join (Free x) = Free $ join <$> x
```

What must be noted here, is that this implementation, while straightforward, is not particularly efficient.
A more efficient implementation, known as Church-encoded free monads, is available.
Naturally, when using free monads, you should prefer the more efficient implementation.
However, the core idea remains the same, so we're not delving into this topic in this introduction.

Free monads, along with free applicative and alternative functors, and cofree comonads, are provided by the `free` package [on Hackage][hackage-link] (and [Stackage][stackage-link]).

[hackage-link]: https://hackage.haskell.org/package/free
[stackage-link]: https://www.stackage.org/lts/package/free

Using free monads, we can define a computation as a data structure.
Here, "computation" is a very broad term.
The data structure in question doesn't define how the computation is performed, and we can write a separate interpreter (or indeed many interpreters) that performs the actual computation in question.

## Using free monads

> Talk is cheap, show me the code
>
> -- Linus Torvalds

With the theory out of the way, let us see some examples to build our intuitions.

### State as a free monad

Let us start off with the `State` monad.
We'll pretend we "forgot" that it's a monad, and try to implement it from scratch in terms of a free monad.

First, we define a typical `State` newtype, and let GHC auto-derive the functor instance:

```haskell
newtype StateF s a = StateF { runStateF :: s -> (a, s) }
  deriving stock Functor
```

(note that we're calling it `StateF` for "state functor")

We can also define the typical functions for working with state:

```haskell
getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ \_ -> ((), s)
```

Now we can define a "free state" monad:

```haskell
type State s = Free (StateF s)
```

We'll also need to lift our state operations into the monad.
We could do it manually, of course.
We have `StateF s a`, and we need to get `StateF s (Free (StateF s) a)`, and wrap it in `Free`.
To lift any `a` into `Free f a` we just need to apply `Pure`:

```haskell
get :: State s s
get = Free $ Pure <$> getF
```

But since it is a common pattern, the `free` library provides a function to do this for us:

```haskell
liftF :: (Functor f, MonadFree f m) => f a -> m a
```

`MonadFree` is just an MTL-style type class for different encodings of the free monad.
In our case, we can pretend that

```haskell
liftF :: Functor f => f a -> Free f a
```

Hence, we can define

```haskell
get :: State s s
get = liftF get

set :: s -> State s ()
set = liftF . set
```

and, like magic, we have a `State` monad:

```haskell
someComputation :: State Int ()
someComputation = do
  i <- get
  put $ i + 1
  pure ()
```

There is a catch, however.
This code will happily compile, but it doesn't do anything interesting.
Indeed, we didn't define the meaning of the computation, only its form, and we can't directly use `runStateF` to "run" our free state monad.

This means we need to write an interpreter, so let's do just that.
Remember that `Free` has two constructors, `Pure` and `Free`, so we need to pattern-match on those:

```haskell
runState :: State s a -> s -> (a, s)
runState (Pure x) s = (x, s)
runState (Free f) s =
  let (m, s') = runStateF f s
  in runState m s'
```

If you're familiar with the usual state monad, you may have noticed that we've essentially moved the implementation of `>>=` to `runState`.
The free monad doesn't specify the meaning of monadic actions, so we have to decide what those actions mean when we're running it.

To illustrate this point, we're going to write another interpreter, which is essentially a pretty-printer:

```haskell
printState :: (Show s, Show a) => State s a -> s -> String
printState (Pure x) s = "pure (" <> show x <> "," <> show s <> ")"
printState (Free m) s =
  let (x, s') = runStateF m s
  in "state change " <> show s <> " -> "<> show s' <> "\n"
    <> printState x s'
```

If we run `someComputateion` defined above through this interpreter, we'll get the following output:

```
state change 1 -> 1
state change 1 -> 2
pure ((),2)
```

Let's summarize what we've learned so far.
We can take any usual base functor for some monad, and get a `Monad` instance "for free".
The caveat is that we need to define the semantics of this monad separately by writing an interpreter.

### List as a free monad

Not all free monads behave exactly the same as their regular counterparts.
For instance, you might know that the list monad encodes non-determinism.
However, if we derive a free monad for lists, the behavior is slightly different.
Consider:

```haskell
listComputation :: FreeList Int
listComputation = do
  x <- liftF [1, 2, 3]
  y <- liftF [70, 80]
  z <- liftF [500, 600]
  pure $ x+y+z

printFreeList :: Show a => FreeList a -> String
printFreeList (Pure x) = show x
printFreeList (Free f) = "["
  <> intercalate "," (printFreeList <$> f)
  <> "]"
```

if we run `printFreeList listComputation`, we will get

```haskell
[[[571,671],[581,681]],[[572,672],[582,682]],[[573,673],[583,683]]]
```

Notice how it gets us what is essentially nested lists, unlike the regular list monad.
Of course, we can get the standard list monad behavior by concatenating the nested lists, but we might not necessarily want to do that.

In general, we can restore any proper monad behavior from a free monad, since it doesn't actually define any semantics, beyond those necessary for any monad.
`free` provides a function for that:

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
```

Given a function[^natural-transformation] converting `f x` into some monad `m x` for any `x`, we can convert `Free f a` into `m a`.
Returning to our list example, we can get the original list behavior by applying `foldFree id`.

[^natural-transformation]: This function is a natural transformation.
    The naturality condition is enforced by parametricity.
    That being said, the discussion of naturality is not particularly relevant here.
    Essentially, you can think of any function with type `forall a. f a -> g a` as a natural transformation between `f` and `g`, as long as you ignore `undefined` and other bottoms.

### Free monads for EDSLs

By now, we hopefully got some intuition for how free monads work.
Let us now define a toy calculator language, which reads a few integers from the standard input, adds them together, and prints the result.

The key intuition here is that if the next action depends on the previous one, we need to encode this as a continuation, i.e. we need to have a function accepting some argument and returning the functor parameter.

One caveat is that in the case of a non-branching computation, we only want our base functor to be a functor in the overall result type, but not in the arguments.
Indeed, we can't define a lawful regular `Functor` instance if the functor argument ends up in the function argument position[^variance].
Hence, we will introduce two type parameters: one for the arguments, and another for the result.

[^variance]: This has to do with `Functor` being a covariant functor, but discussion of variance is out of scope here.

```haskell
data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  deriving Functor
```

Notice that for `Add` we're saying that it takes two arguments `t`, its result is also `t`, but ultimately, the continuation returns the type `a`.
In practice, we will define `add :: t -> t -> Free (ASTF t) t`, but for the purpose of auto-derivation and more flexibility, we're not assuming these types are the same in this definition.
The same line of reasoning applies to `Input`.

With `Output`, we could've defined it as `Output t (() -> a)`, since our output action doesn't have any meaningful result.
But we omit this extraneous argument, and simply encode the continuation as a value.
In practice, we'll set `a` to be `()`.

We can now define our monad and a few helper functions:

```haskell
type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Add x y id

output :: Show t => t -> FreeAST t ()
output x = liftF $ Output x ()
```

and write a simple program:

```haskell
program :: (Read a, Show a) => FreeAST a ()
program = do
  x <- input
  y <- input
  res <- add x y
  output res
```

Now, the interpreter for this program will need to convert our `FreeAST` to `IO`, since we're doing input and output.
We know we can do this with `foldFree`, for that we only need to provide a conversion from the base functor to `IO`:

```haskell
computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
  where
    go :: ASTF Int x -> IO x
    go arg = case arg of
      Add x y next -> pure $ next (x + y)
      Input next -> next . read <$> getLine
      Output x next -> do
        print x
        pure next
```

We can write other interpreters of course, for instance, we could write a pretty-printer here.
Using the fact that we can convert a free monad to any other monad using `foldFree`, we'll do just that.
For instance, let's convert `FreeAST String` into `WriterT String (State Int)`.
We'll be naming variables using a counter in the `State` and producing the printed output using the `Writer`.
Note that the argument type `t` is set to `String` here since our terms will be the variable names, not values.

```haskell
printAST :: FreeAST String () -> String
printAST fast = snd $ evalState (runWriterT $ foldFree go fast) 0
  where
    freshVar :: State Int String
    freshVar = do
      n <- get
      put $ n + 1
      pure $ "x" <> show n
    go :: ASTF String a -> WriterT String (State Int) a
    go f = case f of
      Add x y g -> do
        var <- lift freshVar
        tell $
          var <> " <- add " <> x <> " " <> y <> "\n"
        pure $ g var
      Input x -> do
        var <- lift freshVar
        tell $ var <> " <- input\n"
        pure $ x var
      Output s next -> do
        tell $ "output " <> s <> "\n"
        pure next
```

Notice, up to this point, all constructors of our `ASTF` had exactly one parameter that had anything to do with the functor.
A question you might have now is what happens if we have several.
If you gained a bit of intuition for free monads by now, you might suspect we get a branching computation, and that's exactly right!
Let's add simple branching to our toy language to see how it works.

```haskell
data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  | If t a a
  deriving Functor
```

We add a new constructor, `If`, with one scalar parameter and two branches.
We can now define a helper,

```haskell
if' :: t -> FreeAST t a -> FreeAST t a -> FreeAST t a
if' cond t f = Free $ If cond t f
```

Notice how instead of `liftF` we have to use `Free` directly here -- our branches are already of type `FreeAST`.
We can now modify our interpreter `computeAST` by adding another case:

```haskell
computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
  where
    go :: ASTF Int x -> IO x
    go arg = case arg of
      Add x y next -> pure $ next (x + y)
      Input next -> next . read <$> getLine
      Output x next -> do
        print x
        pure next
      -- below is the only thing that changed
      If cond t f ->
        if cond /= 0 then pure t else pure f
```

Here we define the semantics of the branching -- for the sake of not complicating the example, we interpret `0` as false, and non-zero as true.

When we try modifying the pretty printer, we run into a bit of a problem.
Since our branching operation introduces a bit of non-determinism into the computation, our approach with `WriterT (State Int)` doesn't work anymore.
One could fix it by explicitly adding some kind of non-determinism transformer on top of this, like `ListT` or `LogicT`, but let us instead write the pretty-printer directly.

```haskell
printAST :: FreeAST String a -> String
printAST fast = snd $ evalState (runWriterT $ go fast) 0
  where
    freshVar :: State Int String
    freshVar = do
      n <- get
      put $ n + 1
      pure $ "x" <> show n
    go :: FreeAST String a -> WriterT String (State Int) ()
    go (Pure _) = pure ()
    go (Free f) = case f of
      Add x y g -> do
        var <- lift freshVar
        tell $
          var <> " <- add " <> x <> " " <> y <> "\n"
        go $ g var
      Input x -> do
        var <- lift freshVar
        tell $ var <> " <- input\n"
        go $ x var
      Output s next -> do
        tell $ "output " <> s <> "\n"
        go next
      If cond tru fls -> do
        tell $ "if' " <> cond <> " (do\n"
        _ <- go tru
        tell "\n) (do\n"
        _ <- go fls
        tell "\n)\n"
```

Not much has changed.
Instead of `pure`, we use explicit recursion, and we have to handle the `Pure` case explicitly.
For the sake of simplicity, the return type of `go` is fixed at `()`, since we don't care about the result here.

When we try to run this pretty printer on some code that uses branching, we might be a little surprised by the output.
For example, with the following "program":

```haskell
someAST :: (Read a, Show a) => FreeAST a ()
someAST = do
  x <- input
  y <- input
  res <- if' x (add x y) (pure y)
  output res
```

Applying our pretty-printer, we get:

```haskell
x0 <- input
x1 <- input
if' x0 (do
x2 <- add x0 x1
output x2
) (do
output x1
)
```

The reason for this is simple: when the free monad is built up, the continuation is passed to each place where the base functor is recursive in its parameter.
This means that all the code after our "if'" command gets copied to both branches.
There isn't a workaround for this, since free monads build trees, and not general graphs, however a more efficient representation -- e.g. the Church encoding mentioned previously -- will reduce the overhead.

### Trees as free monads

To close this section out, let's look at another application of free monads.
Namely, let's build a binary search tree from a sorted list.

Our base functor for the binary tree would look like this:

```haskell
data BinTreeF l a = NodeF l a a
  deriving Functor
```

You might ask yourself, where are the leaf and/or nil branch constructors.
We can actually encode those as the "return value" of type `Maybe l`, so we don't really need them here.

Now we define our free monad type and build the tree.

```haskell
type FreeBinTree l = Free (BinTreeF l)

buildBalanced :: [Int] -> FreeBinTree Int (Maybe Int)
buildBalanced [] = pure Nothing
buildBalanced [x] = pure $ Just x
buildBalanced xs = do
  let len = length xs
      (l,x:r) = splitAt (len `div` 2) xs
  b <- liftF $ NodeF x l r
  buildBalanced b
```

The first two cases, where lists have sizes of zero and one respectively, are self-evident.
If there are more elements in the list, we divide it in half.
The approximate "middle" element serves as the current node value.
We then recursively construct left and right subtrees from the corresponding halves of the list.

Notice how we're passing left and right halves of the list to `NodeF` and then bind it to name `b`.
As we discussed previously, multiple functor variables per constructor encode non-determinism.
So here we basically split the computation into two branches.
We then recursively build the tree for each branch.

The caveat here is there's no simple way to print this all, and working with a tree wrapped in the `Free` is probably not particularly convenient.
So we can convert this `FreeBinTree` into a regular binary tree, for example:

```haskell
data BinTree l = Nil | Leaf l | Branch l (BinTree l) (BinTree l)
  deriving Show

convert :: FreeBinTree a (Maybe a) -> BinTree a
convert (Pure Nothing) = Nil
convert (Pure (Just x)) = Leaf x
convert (Free f) =
  let BranchF x l r = convert <$> f
  in Branch x l r
```

Note how `Pure` values correspond to leaves, and `Free` values correspond to branches.
Running `convert . buildBalanced` on a list produces the expected result, i.e. a binary search tree.

```
> convert $ buildBalanced [0..10]
5 (2 (1 0
        Nil)
     (4 3
        Nil))
  (8 (7 6
        Nil)
     (10 9
         Nil))
```

## Conclusions

> The only way to learn a new programming language is by writing programs in it.
>
> -- Dennis Ritchie

Hopefully, after this brief introduction, you understand, at least in principle, what free monads are, and how they may be useful.
Of course, the only way to get proficient with them is to use them in projects, and no blog post can replace actual hands-on experience, but that applies to everything in programming.

Let us then briefly review what we've learned.

- Free monads are "free" because they do not impose any additional constraints beyond those required by the definition of a monad.
- They are a particular type of a free algebraic structure.
- As such, they are very similar to free monoids.
- They build tree-like structures, which later can be interpreted.
- Any typical Haskell monad can be implemented as a free monad with a corresponding interpreter.
- Generally, a free monad can be converted to any other monad via a natural transformation.
- One particular application of free monads is in building ASTs for EDSLs.
- But you could use them almost anywhere where a tree could be used.

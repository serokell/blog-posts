Not all GHC errors are born equal. Some of them are easy to trace and fix,
while some of them are not. And some errors can have variants that span the entire spectrum.

In this article, we'll look at the `overlapping instances` error. We'll understand the many variants of it and what the error means in each of those cases. Along the way, we might also learn
a couple of interesting and advanced things about the behavior of GHC.

## Simple overlapping instances

Let us look at a basic version of this error that is triggered by the following code.

```hs

{-# LANGUAGE FlexibleInstances #-}

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "dummy instance"

instance Printable Int where
  printMe x = putStrLn ("I am an Int with value :" ++ show x)

main :: IO ()
main = printMe (5 :: Int)
```

This gives us the following error:

```
• Overlapping instances for Printable Int
    arising from a use of ‘printMe’
  Matching instances:
    instance Printable a -- Defined at app/Main.hs:8:10
    instance Printable Int -- Defined at app/Main.hs:11:10
• In the expression: printMe (5 :: Int)
```

Note that the error happens where there is a call to `printMe` function. If
there is no call to the function, the error won't be there.

Overlapping instance error is triggered by an instance search, and not by an
instance declaration. You can put all kinds of overlapping instances in your
code, and GHC won't bat an eye until you trigger an instance search, like
when calling a typeclass method.

Let us see what happens in the call site `printMe (5 ::Int)`.  We have two
matching instances in scope. The general instance, `Printable a`, and a
specific instance for `Int`. We call it the 'general instance' because it can
match any type, while the instance for `Int` can only match the `Int` type.

Here GHC chooses to present an error rather than go with the more specific `Int`
instance. This behavior can help the programmer to not accidentally override a
general instance by mistake. It is easy to spot when both instances are in the same
module, but what if the general instance is in another module, or in a
different package? Silently overriding an existing instance or not being
aware of an existing instance while adding a new one could break the program in subtle ways.

### How to fix it?

One way to fix this is to let GHC know that it is alright to choose the
instance in the presence of other matching instances. We do it by using the
`OVERLAPPING` pragma. For example,


```hs
instance Printable a where
  printMe a = putStrLn "dummy instance"

instance {-# OVERLAPPING #-} Printable Int where
  printMe x = putStrLn ("I am an Int with value :" ++ show x)
```

<details>
  <summary>
    Full code
  </summary>

```hs
  {-# LANGUAGE FlexibleInstances #-}

  class Printable a where
    printMe :: a -> IO ()

  instance Printable a where
    printMe a = putStrLn "dummy instance"

  instance {-# OVERLAPPING #-} Printable Int where
    printMe x = putStrLn ("I am an Int with value :" ++ show x)

  main :: IO ()
  main = printMe (5 :: Int)
```
</details>


We can also do it by marking the general instance as being safely overridable
by using the `OVERLAPPABLE` pragma, as shown below.

```hs
instance {-# OVERLAPPABLE #-} Printable a where
  printMe a = putStrLn "dummy instance"

instance Printable Int where
  printMe x = putStrLn ("I am an Int with value :" ++ show x)
```

<details>
  <summary>
    Full code
  </summary>

```hs
  {-# LANGUAGE FlexibleInstances #-}

  class Printable a where
    printMe :: a -> IO ()

  instance {-# OVERLAPPABLE #-} Printable a where
    printMe a = putStrLn "dummy instance"

  instance Printable Int where
    printMe x = putStrLn ("I am an Int with value :" ++ show x)

  main :: IO ()
  main = printMe (5 :: Int)
```
</details>

If you want to mark an instance as being overridable, as well as it being able
to safely override other instances, you can use the `OVERLAPS` pragme. So in
our example, you can use the `OVERLAPS` pragma in either of these instances, and
it will work.

Note that the `OVERLAPPING` pragma just means that it is alright to use that
instance (even if there are other, more general instances) and not an explicit
instruction to prefer that instance.

You can read more about these pragmas [here](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#overlapping-overlappable-overlaps-and-incoherent-pragmas).

## Overlapping instances with type variables

Here we slightly change one of the above fixes to call the `printMe` function
via another function `fn` that accepts a polymorphic argument.

```hs

fn :: a -> IO ()
fn x = printMe x
```

<details>
  <summary>
    Full code
  </summary>


```hs
{-# LANGUAGE FlexibleInstances    #-}

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "general instance"

instance {-# OVERLAPPING #-} Printable Int where
  printMe a = putStrLn "int instance"

fn :: a -> IO ()
fn x = printMe x

main :: IO ()
main = fn (5 :: Int)

```
</details>

Lo and behold, the dreaded error appears again.

```
   • Overlapping instances for Printable a
       arising from a use of ‘printMe’
     Matching instances:
       instance [overlappable] Printable a -- Defined at app/Main.hs:8:32
       instance Printable Int -- Defined at app/Main.hs:11:10
     (The choice depends on the instantiation of ‘a’
      To pick the first instance above, use IncoherentInstances
      when compiling the other instance declarations)
```

Here we have some additional information in the error message. It says
'The choice depends on the instantiation of ‘a’ To pick the first instance
above, use IncoherentInstances'.

So this happens because at the call site of `printMe x`, GHC only knows that `x` can
be of any type, including `Int`. Without knowing if `a` is an `Int` or not,
it cannot pick the most specific instance, causing the error.

### The Fix

We see that `fn` is called with an `Int` argument in the `main` function. So
one can wonder why GHC is not able to figure out that `a` is an `Int` in this
particular call? And they would be right, GHC can, but imagine, if GHC starts
to generate different code for all such polymorphic functions, then there will
be a lot of copies for a single function if it is called with different types.

The solution to this problem is nothing other than the plain old typeclass
constraints.

So if you add a `Printable a` constraint to `fn`, then the proper instance will
be passed from the call site, as a hidden argument (a typeclass dictionary),
and thus the compiler can get away with only generating a single copy of the
`fn` function.

So that is the proper fix in this situtation. Add a `Printable a` constraint to
`fn`.

```hs
fn :: Printable a => a -> IO ()
fn x = printMe x
```

<details>
<summary>Full code</summary>

```hs

{-# LANGUAGE FlexibleInstances #-}

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "general instance"

instance {-# OVERLAPPING #-} Printable Int where
  printMe a = putStrLn "int instance"

main :: IO ()
main = fn (5 :: Int)

fn :: Printable a => a -> IO ()
fn x = printMe x
```

</details>

Another fix, of course, is to mark the `Int` instance as `INCOHERENT`.  This will
pick the instance with the information available at the call site, even if a
different instance is available and could be picked if more information were available.

```hs
instance {-# INCOHERENT #-} Printable Int where
  printMe a = putStrLn "int instance"
```

<details>
<summary>Full code</summary>

```hs

{-# LANGUAGE FlexibleInstances #-}

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "general instance"

instance {-# INCOHERENT #-} Printable Int where
  printMe a = putStrLn "int instance"

main :: IO ()
main = fn (5 :: Int)

fn :: a -> IO ()
fn x = printMe x
```
</details>

The rules followed by the instance resolution algorithm are described
[here](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-IncoherentInstances)
and in this specific case, the general instance will get applied because it
ends up being the single 'prime candidate' which gets selected since the
remaining instance is marked as `INCOHERENT`. This means that the program
will print "general instance" if you run it.

## The 'Shortsighted GHC' overlapping instances

Here we look at a variant of this error where one feels that GHC is sometimes
very short sighted.

```hs
{-# LANGUAGE FlexibleInstances #-}

import Data.Typeable

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe _ = putStrLn "General instance"

instance Functor f => Printable (f a) where
  printMe _ = putStrLn "Instance for a Functor"

newtype MyType a = MyType a

main :: IO ()
main = printMe (MyType 5)

```

As expected, we get the overlapping instances error.

```
• Overlapping instances for Printable (MyType Char)
    arising from a use of ‘printMe’
  Matching instances:
    instance Printable a -- Defined at app/Main.hs:10:10
    instance Functor f => Printable (f a)
      -- Defined at app/Main.hs:13:10
```

Since we saw a similar error in the last example and fixed it by
removing one of the instances, it might appear that same could work here as well.

And it also kind of makes sense. Since GHC is confused by two eligible
instances, in all probability, removing one of them should fix it, right?

So we try by removing the instance for `Printable a`.

<details>
  <summary>Show changed code</summary>

```hs
{-# LANGUAGE FlexibleInstances #-}

import Data.Typeable

class Printable a where
  printMe :: a -> IO ()

instance Functor f => Printable (f a) where
  printMe _ = putStrLn "Instance for a Functor"

newtype MyType a = MyType a

main :: IO ()
main = printMe (MyType 'c')
```
</details>

And when we re-compile, we get...

```
• No instance for (Functor MyType) arising from a use of ‘printMe’
• In the expression: printMe (MyType 'c')
  In an equation for ‘main’: main = printMe (MyType 'c')
```

To our great surprise, we find that removing one instance from the two eligible
instances seems to have made GHC look closer into the remaining instance, and it ended
up rejecting the instance after that. It seems that GHC did not look at the instances well
enough the first time, before declaring them as redundant.

Let us walk through this algorithm and see why the first error happens.

So the very first step is:

>Find all instances $I$ that match the target constraint; that is, the target constraint is a substitution instance of $I$. These instance declarations are the candidates.

Our target constraint here is `MyType Char`, and both the instances for `Printable a`
and `Printable (f a)` match.

The next step says:

>If no candidates remain, the search fails.

Since we have two instances that match, we can continue to the next step, which says,

>Eliminate any candidate $IX$ for which there is another candidate $IY$ such that both of the following hold: $IY$ is strictly more specific than $IX$. That is, $IY$ is a substitution instance of $IX$ but not vice versa. Either $IX$ is overlappable, or $IY$ is overlapping. (This “either/or” design, rather than a “both/and” design, allow a client to deliberately override an instance from a library without requiring a change to the library.)

We have two candidates, `Printable a` and `Printable (f a)`. Let us process
`Printable a` first. The rule says to check if there is another candidate $IY$
such that $IY$ is a substitution instance of $IX$. Here `f a` is a
substituation instance for `a`, because if something can accept `a`, it can accept
`f a` as well, but not the other way around. So it fits, and the next part of the rule
says that either `Printable a` is overlappable, or `Printable (f a)` should
be overlapping. And since this is not the case, we cannot eliminate `Printable a`.

The next one is `Printable (f a)`, and we cannot eliminate it since the other
instance `a` is not a substitution instance of `f a`. If something is expecting `f a`
you cannot give `a` to it. Or in other words, `a` is not more specific than `f a`, but
instead, it is more general.

So after this rule, both instances remain, and the next rule says:

>If all the remaining candidates are incoherent, the search succeeds, returning an arbitrary surviving candidate.

None of our instances are marked with the `{-# INCOHERENT #-}` pragma, so we proceed to the next rule.

>If more than one non-incoherent candidate remains, the search fails.

Considering that we have two such instances now, the lookup fails here.

Let us try adding an `OVERLAPPING` pragma to the instance for `f a`.

```
instance {-# OVERLAPPING #-} Functor f => Printable (f a) where
  printMe _ = putStrLn "Instance for a Functor"
```

And now we get the error:

```
• No instance for (Functor MyType) arising from a use of ‘printMe’
• In the expression: printMe (MyType 'c')
  In an equation for ‘main’: main = printMe (MyType 'c')
```

We can see that adding `OVERLAPPING` pragma enabled the elimination of the
instance for `Printable a` at step 3. But the remaining instance `Functor f =>
Printable (f a)` failed to work, because `MyType` is not a `Functor`. But this
failure happens at a later phase: when constraints are matched and after GHC
has picked an instance. This is why we get a `No instance for Functor MyType`
error instead of an overlapping instance error.

This is also what happens if we remove the general instance `instance Printable a`.

Here we come across a crucial aspect of instance resolution: the algorithm
works in two distinct steps and it never backtracks.

In the first step, it does not look at constraints at all, only instance heads.

So instead of:


```
instance Printable a
instance Functor f => Printable (f a)
```

It sees:

```
instance Printable a
instance ... => Printable (f a)
```

And it has to pick an instance for the next step on this information alone.
In the next step, constraints are matched.

So in this example, when we add an `OVERLAPPING` pragma, it made the first step
to complete successfully with the instance `Functor f => Printable (f a)` as
result. But in the context matching step, this instance failed, because
`MyType` is not a `Functor`.

And since GHC does not backtrack, if will not go back to first step with the memory
of this failure, and pick the general instance. Understanding this two step process
with the no-backtracking behavior is crucial in untangling most occurances
of this error.

### How to fix it?

We can either remove the instance for `f a`, which makes the algorithm pick the
instance for `Printable a`. Or else we can add a Functor instance for `MyType a`
if it makes sense.

## Poly-kinded overlapping instances

This is a verison of overlapping instances error that only happens with
`PolyKinds` language extension and automatic kind inference that comes with it.

To demonstrate this, we unfortunately need a bit more elaborate setup, and
frankly, this example is a bit contrived. Anyway, so we have this code below
which triggers our beloved error:

```hs
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

import Data.Proxy
import GHC.TypeLits

class Printable n a where
  printMe :: Proxy n -> a -> IO ()

class SomeClass (n :: Symbol) a where

instance SomeClass n (Maybe Char) where

instance Printable n a where
  printMe p a = putStrLn "General instance"

instance {-# OVERLAPPING #-} SomeClass n (Maybe a) => Printable n (Maybe a) where
  printMe p a = putStrLn "Specific instance"

fn :: Proxy n -> Maybe Char -> IO ()
fn p a = printMe p a

main :: IO ()
main = fn Proxy (Just 'c')
```

As you can see, apart from enabling a bunch of language extensions, we have
added a `Proxy` argument to the `printMe` method. It serves no other purpose
other than to trigger and demonstrate the error.

Here we have these two instances,

```hs
instance Printable n a where
  printMe p a = putStrLn "General instance"

instance {-# OVERLAPPING #-} SomeClass n (Maybe a) => Printable n (Maybe a) where
  printMe p a = putStrLn "Specific instance"
```

And as per what we have seen already, the second instance should be
selected in the call to `printMe p (Just 'c')` because it has the `OVERLAPPING`
pragma and it appears to be the more specific instance.

But nevertheless, here is the error:

```
||     • Overlapping instances for Printable n (Maybe Char)
||         arising from a use of ‘printMe’
||       Matching instances:
||         instance forall k (n :: k) a. Printable n a
||           -- Defined at app/Main.hs:22:10
||         instance [overlapping] SomeClass n (Maybe a) =>
||                                Printable n (Maybe a)
||           -- Defined at app/Main.hs:25:30
||       (The choice depends on the instantiation of ‘k, n’
||        To pick the first instance above, use IncoherentInstances
||        when compiling the other instance declarations)
||     • In the expression: printMe p a
||       In an equation for ‘fn’: fn p a = printMe p a
```

Let us look closer at the second instance.

```
instance {-# OVERLAPPING #-} SomeClass n (Maybe a) => Printable n (Maybe a) where
```

Here it appears that the kind of `n` can by any kind, but the `PolyKinds`
extension and constraint `SomeClass n (Maybe a)` causes the kind inference
system to infer that type `n` must be of kind `Symbol`. And at the call site,
in the `fn` function, we don't know the kind of `n`. If it is of kind `Symbol` then
the second instance should be called, but if it something else, then the first
instance should be called. And this dilemma makes GHC give up and produce the
error.

### How to fix it?

We can see the error disappear once we remove the `SomeClass n (Maybe a)`
constraint from the second instance. Alternatively, we can keep the constraint
and kind annotate the proxy from the call site. For example, the following
change to the call site will fix the error and call the first (general) instance.

```hs
fn :: Proxy (n :: *) -> Maybe Char -> IO ()
fn p a = printMe p a
```
And the following will fix it and call the second (specific) instance.

```hs
fn :: Proxy (n :: Symbol) -> Maybe Char -> IO ()
fn p a = printMe p a
```

## Conclusion

Here we saw some commonly occurring instances of the Overlapping Instances error
that GHC seemingly loves to present us now and then. Hopefully, we have learned
a thing or two about how GHC resolves type class instances, which might help us
track down and fix the error properly the next time we come across it.

If you would like to read more of our Haskell articles, be sure to check our [Haskell section](https://serokell.io/blog/haskell) or follow us on [Twitter](https://twitter.com/serokell) or [Dev](https://dev.to/serokell).

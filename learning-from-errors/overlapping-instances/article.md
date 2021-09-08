Not all GHC errors are born equal. Some of them are easy to trace and fix,
while some of them are not so. And some errors like the 'overlapping
instances', can have variants that span the entire spectrum. Here we look at
what this error mean and many of its variants. Along the way, we might learn
a couple of intersting and advanced things about the behavior of GHC.

## Simple overlapping instances

```
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "dummy instance"

instance Printable Int where
  printMe x = putStrLn ("I am an Int with value :" ++ show x)

main :: IO ()
main = printMe (5 :: Int)
```

This gives us the error

```
• Overlapping instances for Printable Int
    arising from a use of ‘printMe’
  Matching instances:
    instance Printable a -- Defined at app/Main.hs:8:10
    instance Printable Int -- Defined at app/Main.hs:11:10
• In the expression: printMe (5 :: Int)
```

Note that the error happens where there is a call to `printMe` function. If
there is no call to the function, the error won't be there. ( overlapping
instance error is triggered by a function call, and not by an instance
declaration.  You can put all kinds of overlapping instances in your code, and
GHC won't bat an eye until you try to call a method in the respective
typeclass)

Let us see what happens in the call site `printMe (5 ::Int)`.  We have two
matching instances in scope. The general instance for `Printable a`, and an
specific instance for `Int`. We call it a 'general instance' because it can
match any type, while the instance for `Int` can only specifically match the
`Int` type.

Here GHC chooses to throw an error, rather than go with the more specific `Int`
instance. This behavior could help the programmer to not accidentally override a
general instance. It is easy to spot when both instances are in the same
module, but what if the general instance is in another module, or in a
different package. Silently overriding an existing instance, or not being
aware of an existing instance could break the program in subtle ways.

### The Fix

One way to fix this is to let GHC know that it is alright to choose the
instance in the presence of other matching instances. We do it by using the
`OVERLAPPING` pragma. For example,


```
instance Printable a where
  printMe a = putStrLn "dummy instance"

instance {-# OVERLAPPING #-} Printable Int where
  printMe x = putStrLn ("I am an Int with value :" ++ show x)
```

We can also do it by marking the general instance as being safely over ridable
by using the `OVERLAPPABLE` pragma, as shown below.

```
instance {-# OVERLAPPABLE #-} Printable a where
  printMe a = putStrLn "dummy instance"

instance Printable Int where
  printMe x = putStrLn ("I am an Int with value :" ++ show x)
```

If you want to mark an instance as being overridable, as well as it being able
to safely override other instances, you can use the `OVERLAPS` pragme. So in
our example, you can use the `OVERLAPS` pragma in either of these instances and
it will work.

Note that 'OVERLAPPING' pragma just means that it is alright to use that
instance, if there are other, more general instances, and not an explicit
instruction to prefer that instance.

Read more about these pragmas [here](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#overlapping-overlappable-overlaps-and-incoherent-pragmas)

## Overlapping instances with type variables

Here we slightly change one of the above fixes, to call the `printMe` function
via another function `fn` that accepts a polymorphic argument.

```
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

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
lo and behold the dreaded error appears again.

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

So this happens because at the call site of `printMe x` GHC only know `x` can
be of any type, including `Int`. Without knowing if `a` is an `Int` or not,
it cannot pick the most specific instance, causing the error.

### The Fix

One fix of course is to mark the `Int` instance as `INCOHERENT`.

```

{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

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

The rules followed by the instance resolution algorithm are described
[here](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-IncoherentInstances)
and in this specific case, the general instance will get applied because it
ends up being a single 'prime candidate' which gets selected since the
remaining instance is marked as 'INCOHERENT'. This means that the program
will print "general instance" if you run it.

But what if you want the `Int` instance to be called instead? One way is to use
`Typeable` and prove to the compiler that `a` is in fact, and `Int`. And once
you prove that `a` is an `Int` GHC will happily call the `printMe` in the `Int`
instance for you, of course at the cost of including `Typeable` constraint in
the signature of `fn` function (and a small runtime cost because of the Typeable
constraint to the function).

```
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Typeable (cast)

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "general instance"

instance {-# INCOHERENT #-} Printable Int where
  printMe a = putStrLn "int instance"

main :: IO ()
main = fn (5 :: Int)

fn :: forall a. Typeable a => a -> IO ()
fn x = case cast x of
  (Just a :: Maybe Int) -> printMe a
  Nothing               -> printMe x
```

Similarly we could change/hack the general instance to use `Typeable` and implement
specialized behavior for `Int` in the general instance itself.

```
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Typeable

class Printable a where
  printMe :: a -> IO ()

instance Typeable a => Printable a where
  printMe a = case cast a of
    (Just x :: Maybe Int) -> putStrLn "general instance for int"
    Nothing               -> putStrLn "general instance"

main :: IO ()
main = fn (5 :: Int)

fn :: forall a. Typeable a => a -> IO ()
fn x = printMe x
```

## Flip-Flop Overlapping instances

Here we look at a variant of this error that seemingly flip flops when
attempts are made to fix it. Let us look at a sample.

```
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

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

Since we saw a similar error in the last example, and we could have fixed it by
removing one of the instances, it might appear that same could work here as well.

And it also kind of makes sense, since GHC is confused by two eligebile instances,
so in all probablity, removing one of them should fix it, right?

So we try by removing the instance for `Printable a`, as below,

```
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Data.Typeable

class Printable a where
  printMe :: a -> IO ()

instance Functor f => Printable (f a) where
  printMe _ = putStrLn "Instance for a Functor"

newtype MyType a = MyType a

main :: IO ()
main = printMe (MyType 'c')
```
and re-compile, and we get..

```
• No instance for (Functor MyType) arising from a use of ‘printMe’
• In the expression: printMe (MyType 'c')
  In an equation for ‘main’: main = printMe (MyType 'c')
```

To our great surprise, we find that removing one instance from the two eligible
instances made GHC reject the remaining instance as well!

So we end up in this situtation where where GHC flip-flops between these two errors.

It shows that it might not be a good idea to blindly remove instances when you
come across overlapping instance errors. It is better to carefully examine the
actual cause of the issue, keeping in mind the algorith GHC follow to resolve
instances.

Let us walk through this algorithm and see why the first error happens.

So the very first step is:

```
Find all instances I that match the target constraint; that is, the target
constraint is a substitution instance of I. These instance declarations are the
candidates.
```

Our target constraint here is `MyType Char`, and both the instances for `Printable a`
and `Printable (f a)`, matches.

The next step says.

```
If no candidates remain, the search fails
```

Since we have two instances that match, we can continue to the next step, which says,

```
Eliminate any candidate IX for which there is another candidate IY such that
both of the following hold: IY is strictly more specific than IX. That is, IY
is a substitution instance of IX but not vice versa. Either IX is overlappable,
or IY is overlapping. (This “either/or” design, rather than a “both/and”
design, allow a client to deliberately override an instance from a library,
without requiring a change to the library.)
```

We have two candidates `Printable a` and `Printable (f a)`. Let us process
`Printable a` first. The rule says to check if there is another candidate `IY`
such that `IY` is a substitution instance of IX. Here `f a` is a
substituation instance for `a`, because if something can accept `a`, it can accept
`f a` as well, but not the other way around. So it fits, and the next part of the rule
says that either `Printable a` is overlappable, or `Printable (f a)` should
be overlapping. And since this does not match, we cannot eliminate `Printable a`.

The next one is `Printable (f a)`, and we cannot eliminate it since the other
instance `a` is not a substitution instance of `f a`. If something is expecting `f a`
you cannot give `a` to it. Or in other words, `a` is not more specific than `f a`, but
instead it is more general.

So after this rule, both instances remain, and the next rule says

```
If all the remaining candidates are incoherent, the search succeeds, returning an arbitrary surviving candidate.
```

None of our instances are marked `Incoherent` so we proceed to the next rule.

```
If more than one non-incoherent candidate remains, the search fails.
```

And here the lookup fails.

Let us try adding an `OVERLAPPING` pragma to the instance for `f a`.

```
instance {-# OVERLAPPING #-} Functor f => Printable (f a) where
  printMe _ = putStrLn "Instance for a Functor"
```

And now we get the error

```
• No instance for (Functor MyType) arising from a use of ‘printMe’
• In the expression: printMe (MyType 'c')
  In an equation for ‘main’: main = printMe (MyType 'c')
```

We can see that adding `OVERLAPPING` pragma enabled the elimination of
instance for `Printable a` at step 3. But the remaining instance `Functor f => Printable (f a)`
failed at the last step which says.

```
Now find all instances, or in-scope given constraints, that unify with the
target constraint, but do not match it. Such non-candidate instances might
match when the target constraint is further instantiated. If all of them are
incoherent top-level instances, the search succeeds, returning the prime
candidate. Otherwise the search fails.
```

Here we come across a crucial aspect of instance resolution, that the algorithm
never backtracks. When the algorithm failed at the last step, if it could
backtrack, it could have picked the instance for `Printable a` which was
eliminated at step 3, in favor of the failed instance for `f a`. But instead, the
algorithm just fails.

### The Fix

We can either remove the instance for `f a`, which makes the algorithm pick the
instance for `Printable a`. Or else we can add a Functor instance for `MyType a`
if it makese sense.

Possible references:

https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-icfp2010-instances.pdf

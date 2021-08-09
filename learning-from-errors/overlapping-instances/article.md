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

instance {-# OVERLAPPABLE #-} Printable a where
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
there is no call to the function, the error won't be there. Let us see what
happens in the call site `printMe (5 ::Int)`.  We have two matching instances
in scope. The general instance for `Printable a`, and an specific instance for
`Int`. We call it a 'general instance' because it can match any type, while the
instance for `Int` can only specifically match the `Int` type.

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

instance {-# INCOHERENT #-} Printable Int where
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

Here we have some additional information in the error message, where it says
'The choice depends on the instantiation of ‘a’ To pick the first instance
above, use IncoherentInstances'.

So this happens because at the call site of `printMe x` GHC only know `x` can be
of any type, including `Int`, so it wouldn't know if it should call the `printMe`
in the `Int` instance or the one in the general instance, causing the error.

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
the signature of `fn` function.

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
attempts are made to fix it. Let us look at a sample, which involves
a typeclass called `convert`.

```
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Data.Typeable

class Convertable a b where
  convert :: a -> b

instance Integral a => Convertable a Int where
  convert = fromIntegral

instance {-# OVERLAPPING #-} Integral a => Convertable Int a where
  convert = fromIntegral

main :: IO ()
main = putStrLn $ show (fn 10 :: Integer)

fn :: (Show a, Integral a) => Int -> a
fn x = convert x
```

As expected, we get the overlapping instances error.

```
• Overlapping instances for Convertable Int a
        arising from a use of ‘convert’
      Matching instances:
        instance Integral a => Convertable a Int
          -- Defined at app/Main.hs:11:10
        instance [overlapping] Integral a => Convertable Int a
          -- Defined at app/Main.hs:14:30
      (The choice depends on the instantiation of ‘a’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
```

Since we had a similar error in the last example, and we could have fixed it by
removing one of the instances, it might appear that same could work here as well.

And it also kind of makes sense, since GHC is confused by two eligebile instances,
so in all probablity, removing one of them should fix it, right?

So we try by removing the instance for `Convertable Int a`, as below,

```

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Data.Typeable

class Convertable a b where
  convert :: a -> b

instance Integral a => Convertable a Int where
  convert = fromIntegral

main :: IO ()
main = putStrLn $ show (fn 10 :: Integer)

fn :: (Show a, Integral a) => Int -> a
fn x = convert x
```
and re-compile, and we get..

```
    • Could not deduce (Convertable Int a)
        arising from a use of ‘convert’
      from the context: (Show a, Integral a)
        bound by the type signature for:
                   fn :: forall a. (Show a, Integral a) => Int -> a
```

To our great surprise, we find that removing one instance from the two eligible
instances made GHC reject the remaining instance as well!

So we end up in a "damned if you do, damned if you don't" situtation where GHC
flip-flops between these two errors.

It shows that it might not be a good idea to blindly remove instances when you
come across overlapping instance errors. It is better to carefully examine the
actual cause of the issue.

After we look at the instances and the call to `convert` function, we can see
that when both instances are present, it triggers an overlapping instance error
in the same way we saw in the example in the last section; without knowing
`a`, the most specific instance cannot be selected.

When we remove the instance for `Convertable Int a`, it means that the remaining
instance for `Convertable a Int` will work only if `a` is Int. But since the function
has to work with any type for `a`, it triggers a 'cannot deduce' error.

### The Fix

After lookin closer it becomes clear that we removed the wrong instance. And when we
remove the instance for `Convertable a Int`, it works, since the remaining instance
`Convertable Int a` matches the required instance exactly.

Possible references:

https://homepage.cs.uiowa.edu/~jgmorrs/pubs/morris-icfp2010-instances.pdf

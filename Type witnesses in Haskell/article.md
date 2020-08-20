## Haskell type-level witness

In this article, we look at the concept of type witness.  The target audience
of this post is people who are still new to extensions such as `DataKinds`,
`GADTs`, `ExistentialQuantification` and are wondering how it all fits
together. We will look at how a type witness can be useful and also,
hopefully, gain a little more intuition into the workings of the aforementioned
type system extensions along the way.

If you read articles that describe some advanced type-level stuff in
Haskell or other languages with a similar type system, chances are that you have
come across this thing called 'type witness' or 'runtime evidence'. In this
post, we will be trying to gain an understanding of what the heck it is.

In simple terms, a runtime witness is a value that in some way holds some type-level
information associated with a polymorphic value and makes it available to the type
checking process.

But this is confusing because type checking happens at compile time and
values are often available only at runtime.  So how can values provide type
information at compile time?

It is possible because even though values are only available at runtime, if
there is a branch in the code (if-then-else, case statements) that branches on a value,
we can make assumptions about that value inside each of the branches.

So, for example, if there is a branch in code like:

```
if (i == 1) then { -- block 1 -- } else { -- block 2 -- }
```

we can safely assume that if we find ourselves in block 1, then `i` will be 1
inside that block and if we find ourselves in block 2, then `i` was not 1.

Thus, at compile time, we will have some information about a value in
conditional branches of the code that branches on the said value. The core
idea of the type witness technique is to use this information to make the compiler
infer the attributes of a polymorphic type, such as what the inferred
type is, how it is constrained, etc.

For this, we first need a way to link a value to some type-level
detail. In Haskell, we have the `GADTs` extension, which enables us to define a
data type of the form

```
data MyData a where
  MyValue1 :: MyData Int
  MyValue2 :: MyData String
  MyValue3 :: MyData Char
```

The powerful thing about this kind of data definitions is that it enables us
to explicitly mark the constructors to be of a certain concrete type.

And thus, we have declared values `MyValue1`, `MyValue2`, and `MyValue3` to have
types `MyData Int`, `MyData String`, and `MyData Char` respectively.
Therefore, these values can now point to what the type `a` in `MyData a` is.

Now, let us see how this type can act as a "witness".

Consider the following function. You can see that by branching on the
`MyData a` value, we are able to figure out what `a` is.

```
func1 :: MyData a -> a
func1 myData =
  case myData of
    MyValue1 -> 10 -- `a` is Int here
    MyValue2 -> "I am a string" -- `a` is String here
    MyValue3 -> 'c' -- `a` is Char here
```

But how is the name "witness" justified? What is it witnessing?

Imagine that this function is part of an expression, say

```
(10 :: Int) + (func1 MyValue1)
```

The `MyValue1` constructor, by being a part of the expression, is *witnessing* the
type checker's type inference of expression `func1 MyValue1` to be an `Int`.
Hence the name "type witness". We can have witnesses that witness other
things, like `a` is the same type as `b`, or `a` has been constrained in a certain
way, etc.

### Singletons

We saw that the values of `MyData a` can point to what `a` is. In addition,
since each polymorphic variant of `MyData a` contains one and only one value,
the concrete types of `MyData a` can also point to value, because there is only
one possible value for any given variant. Such types where there is a one-to-one
correspondence between values and types are called Singletons.

## So how is this useful / Why should I care?

Static type systems can feel very restrictive at the beginning, but, if they are
sufficiently advanced, you will find that you can get some of that
flexibility of dynamically typed languages back while retaining the safety of
static typing.

Let’s see an example where this is manifested, which also involves the use of a
type witness.

Imagine you are building an application that has got users with different privileges.
We represent the possible privilege using a type,

```
data UserPrivilege = Member | Admin | Guest
```

and the users can now be represented by something like,

```
data User = User { userId :: Integer, userName :: String, userPrivilege :: UserType }
```

Since we are interested in type safety, we want to make the `userPrivilege` attribute to be
at the type level, so that if we pass a user of privilege `Member` to a function that
requires a user with privilege `Admin`, the compiler will catch it at compile time.

To do this, we add a type argument to the `User` type. We also enable the `DataKinds`
extension so that the constructors of `UserPrivilege` will be available at the type level
to tag the `User` type with. So, we end up with something like,

```
data User (ut :: UserType) = User { userId :: Integer, userName :: String }
```

Now, we have the user privilege at the type level, and this will prevent us from passing
`User 'Member` to a function that requires `User 'Admin`.

But we find it is now impossible to write a function that reads a user from the
database without explicitly specifying which privilege the user has. So for example,
we try to implement this function with the following type.

```
fetchUserById :: Int -> IO (User a)
```

But this is not possible because if you read the user from the database and
find the user to be of type 'member', you won't be able to return the concrete
type `User 'Member` from the function, because the signature says that it
should be able to return `User a` *for all* `a`.

The idea of a polymorphic value `User a` is that it should be able to concretize into
any type, *as required by the expression where the polymorphic value is used*.
So here, in the `fetchUserById` function, if we find the user read from the database to
have a privilege of `Admin`, we can return the concrete value only after checking that
the caller of this function is indeed asking for `User 'Admin`. We have seen how it can
be done in the `func1` function we saw earlier. But here, we won't be able to use something
like that, simply because we wouldn't know the privilege of the user when we make
the `fetchUserById` call.

One solution to this problem is to wrap the `user` type in another type, which will have
multiple constructors, each wrapping a different type of user, thus hiding the type-level
privilege behind them.

```
data UserWrapper
  = MemberUser (User 'Member)
  | AdminUser (User 'Admin)
  | GuestUser (User 'Guest)
```

A problem with this approach is that you will have to match on all these constructors
every time you read a `user` from db to do anything with it, even when you don't care about the
privilege of the user.

Another way to hide the type-level privilege is by using a `GADT` wrapper type that hides the type-level
privilege behind a GADT constructor.


```
data SomeUser where
  SomeUser :: forall a. User a -> SomeUser
```

Since the `SomeUser` type constructor does not have the type parameter, we can
wrap it around a `User a` of any privilege and return from our database read
function.

But now, we will find that the `User a` that is unwrapped from the `SomeUser` type can only be
used with functions that accept a polymorphic user, that is, `User a`, and cannot be used with
a function that requires concrete types, such as  `User 'Admin`.

This is exactly what we wanted in the first place. We are prevented from
passing a user of unknown privilege to a function that requires an administrator
privilege. But it seems that now we cannot make that call at all.  How can we
convince the type checker that the `User a` unwrapped from `SomeUser` is in fact
`User 'Admin`?

We can do that by using a type witness. We add the following type to act as a witness.

```
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege Member
  WitnessGuest :: WitnessPrivilege Guest
  WitnessAdmin :: WitnessPrivilege Admin
```

Then we change the `User` type to include this witness as one of its fields.

```
data User (up :: UserPrivilege) = User
  { userId :: Integer
  , userName :: String
  , userPrivilege :: WitnessPrivilege up
  }
```

And that is it. When you want to convert a `User a` unwrapped from `SomeUser`
to a concrete type, like `User 'Admin`, you only have to pattern match on
the `userPrivilege` field. As soon as you get a match on the `WitnessAdmin`
branch, GHC will have inferred the `User a` to be an `User 'Admin`, and allow you to
call functions that require `User 'Admin`.

Thanks to the included type witness, we get the best of both worlds;
a type-level user privilege which gets out of
the way when you don't need it, but can pop up anytime you need it.

### Full code sample

```
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}

module Main where

import Data.List

-- User privileges for our users
data UserPrivilege = Member | Admin | Guest

-- Our type witness
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege Member
  WitnessGuest :: WitnessPrivilege Guest
  WitnessAdmin :: WitnessPrivilege Admin

-- Our user type
data User (up :: UserPrivilege) = User
  { userId :: Integer
  , userName :: String
  , userPrivilege :: WitnessPrivilege up
  }

-- The type that we use to hide the privilege type variable
data SomeUser where
  SomeUser :: User a -> SomeUser

-- A function that accept a user id (Integer), and reads
-- the corresponding user from the database. Note that the return
-- type level privilege is hidden in the return value `SomeUser`.
readUser :: Integer -> IO SomeUser
readUser userId = pure $ case find ((== userId) . (\(a, _, _) -> a)) dbRows of
  Just (id_, name_, type_) ->
    case type_ of
      "member" -> SomeUser (User id_ name_ WitnessMember)
      "guest" -> SomeUser (User id_ name_ WitnessGuest)
      "admin" -> SomeUser (User id_ name_ WitnessAdmin)
  Nothing -> error "User not found"

-- This is a function that does not care
-- about user privilege
getUserName :: User up -> String
getUserName = userName

-- This is a function only allows user
-- with Admin privilege.
deleteStuffAsAdmin :: User 'Admin -> IO ()
deleteStuffAsAdmin _ = pure ()

main :: IO ()
main = do
  (SomeUser user) <- readUser 12

  putStrLn $ getUserName user -- We don't care about user privilege here

  case userPrivilege user of -- But here we do.
    -- So we bring the type-level user privilege in scope by matching
    -- on `userPrivilege` field and then GHC knows that `user`
    -- is actually `User 'Admin`, and so we can call `deleteStuffAsAdmin`
    -- with `user`.
    WitnessAdmin ->
      deleteStuffAsAdmin user
    _ -> error "Need admin user"

dbRows :: [(Integer, String, String)]
dbRows =
  [ (10, "John", "member")
  , (11, "alice", "guest")
  , (12, "bob", "admin")
  ]
```

### Recomended further reading

1. [Introduction to Singletons series](https://blog.jle.im/entry/introduction-to-singletons-1.html)
2. [Dependently typed programming with singletons](https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1009&context=compsci_pubs)
3. [Dimensions and Haskell: Singletons in Action](https://serokell.io/blog/dimensions-haskell-singletons)
4. [Why Dependent Haskell is the Future of Software Development](https://serokell.io/blog/why-dependent-haskell)

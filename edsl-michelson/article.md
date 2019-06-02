# Parsing typed eDSL

Embedded DSL (or eDSL) is a popular technique for encoding your domain specific language into Haskell’s type system. One example of such DSL is Ivory -- eDSL for C code generation. Even more often it’s useful to implement your DSL as Haskell data type and interpret it right from Haskell.

Michelson is a smart contract language from tezos community. Akin to Forth, Michelson contract is described by a sequence of instructions operating on a typed stack. Each instruction assumes stack of certain type as input and produces an output stack of determined type. For example, `PAIR` instruction presumes stack of type `a : b : s` and produces stack of type `pair a b : s` for any stack tail `s`.
You can read more about Michelson instructions and typing in the [official documentation](http://tezos.gitlab.io/zeronet/whitedoc/michelson.html).

In January 2019 in collaboration with Toquiville group of Tezos foundation Serokell started [Morley project](https://gitlab.com/morley-framework/morley/).
One of Morley’s goals is to implement a comprehensive framework for testing arbitrary Michelson contracts. This testing should support simple unit testing (when contract is fed with particular sets of input and output values) as well as more complex property-based testing. It was decided to use Haskell for implementation of Morley and as a first step we implemented Michelson language as a very simple AST data type:

```haskell
data T =
   Tint
 | Tnat
 | Toption T
 | Tlist T

data UVal =
   UInt  Integer
 | USome UVal
 | UNone
 | UList [UVal]

data UInstr =
   UNOP
 | UDROP
 | UDUP
 | USWAP
 | UPUSH T UVal
 | UPAIR
 | UCAR
 | UCDR
 | UADD
 | UCONS
 | UNIL T
 | UIF_CONS
```

Soon we understood that this simple AST suffers from certain limitations. One issue was that it was untrivial to generate arbitrary loosely-typed values. In our AST list was merely a constructor `UList [UVal]` and we couldn’t write an `Arbitrary` instance that would generate an arbitrary list of integers or strings depending on type context.

An answer to this problem was obvious: create an AST with stronger types. Expression became annotated with a type, which is easy to implement thanks to awesome GADTs extension. This immediately solves the problem of arbitrary value generation. And moreover, it becomes possible for interpreter to stop unpredictably failing with runtime type errors.

```haskell
data Val t where
  VInt :: Int -> Val 'TInt
  VNat :: Word -> Val 'TNat
  VList :: [Val t] -> Val ('TList t)
  VPair :: Val p -> Val q -> Val ('TPair p q)
```

But after introducing this type we quickly found ourselves at a challenge. It was very easy to parse textual code representation to simple AST, but is obscure how to do the same for the typed representation of Michelson. To simplify things instead of parsing we’ll consider a task of conversion from simple AST to typed AST.

First problem with conversion from simple AST to typed representation lies in the fact that conversion of parent branch in AST depends on the types of children. A useful trick for solving this issue can be found in the [blog post from 2009](http://tezos.gitlab.io/zeronet/whitedoc/michelson.html).
In short, we create an existential type which holds value along with its type and return this existential type from our type check function:

```haskell
data Sing (t :: T) where
  STInt  :: Sing 'TInt
  STNat  :: Sing 'TNat
  STList :: Sing t -> Sing ('TList t)
  STPair :: Sing p -> Sing q -> Sing ('TPair p q)

data SomeVal1 where
  SomeVal1 :: Val t -> Sing t -> SomeVal1

typeCheckVal1 :: UVal -> Maybe SomeVal1
typeCheckVal1 (UInt i) = Just $ SomeVal1 (VInt i) STInt
typeCheckVal1 (UPair p q) = do
  SomeVal1 pv pt <- typeCheckVal1 p
  SomeVal1 qv qt <- typeCheckVal1 q
  pure $ SomeVal1 (VPair pv qv) (STPair pt qt)
typeCheckVal1 (UList _) = error "not implemented"
```

Data type `Sing` can be derived automatically with use of [singletons](http://hackage.haskell.org/package/singletons) library. That library provides `Sing` data family and some useful helper functions and classes for work with singletons.
Throught this article we'll stick to handwritten `Sing` and conversion functions.

There are two major problems with such construction.
First, a reader may have noticed that neither `STNat` nor `VNat` constructors were ever used.
Indeed, `UInt` constructor from simple AST was meant to represent both signed and unsigned integers because they roughly the same representation.
We can not really distinguish between `TInt` and `TNat` literals during parsing.

Similar issue appears in case of list constructor with empty list wrapped in it.
When given a list constructor, we have no idea what type of values this list contains.
In case of empty list, we must return type `forall t. TList t`, but our type representation has
no support for such construction.

Second problem with this snippet is of similar nature.
In case of non-empty list we can take `t` as type of the first element in a list and compare that other elements in
the list have the same type. But to compare type `t1` of first element of a list to the type `t2` of second element of the list, we need `Typeable t1`, `Typeable t2` constraints to hold.

It's relatively easy to address the second problem. We introduce `SomeVal` data type with `Typeable` constraint
put in the scope of constructor:

```haskell
data SomeVal where
 SomeVal :: Typeable t => Val t -> Sing t -> SomeVal
```

First problem requires a switch to different approach for type checking.
One way to solve the problem is to introduce some sort of constrained `forall` quantifier into
our simplistic type system, similarly to what we have in Haskell.
This way for an empty list case we can write something like `SomeVal (VList []) (forall n. Num n => n)`.
This approach is more universal, but significantly heavier to implement and maintain.

Luckily for us, a lighter approach is possible for type checking Michelson programs.
Despite the fact that all Michelson instructions are polymorphic, Michelson programs
are always given in context of a contract.
A contract defines input stack type and each instruction modifies stack type in umambiguous way.
Hence there is no actual need to implement type checking algorithm that derives a type (with some `forall` quantifiers)
for an arbitrary sequence of Michelson instructions.
What we are going to do is to start from the input stack type and iterate through instructions.
This way we'll be able to stick to type system represented by data type `T`.

In the example above we considered only type checking of values.
Following rule defined above, we'll implement `typeCheckVal` function with first argument being a type
of an expression we're trying to parse. There are only two instructions that introduce new value to the stack
(namely, `UPUSH` and `UNIL`) and both of them explicitely have type representation included.

```haskell
typeCheckVal :: Sing t -> UVal -> Maybe (Val t)
typeCheckVal STInt (UInt i) = Just (VInt i)
typeCheckVal STNat (UInt i) = do
  guard (i >= 0)
  pure (VNat $ fromIntegral i)
typeCheckVal (STPair pt qt) (UPair p q) = do
  pv <- typeCheckVal pt p
  qv <- typeCheckVal qt q
  pure $ VPair pv qv
typeCheckVal (STList t) (UList l) =
  VList <$> mapM (typeCheckVal t) l
typeCheckVal _ _ = Nothing
```

Note, that we do not actually need to wrap `Val t` into `SomeVal` here, because first argument defines well the output of the function.
It's important to emphasize the role of singletons in the construction above. What we do is pattern-matching on the type
of value that should be parsed. It's not common in Haskell to pattern-match on type in code of term-level functions and singletons is perhaps the most straightforward way.

Now, let's implement conversion for instructions. First, we'll have to slightly modify our `Sing` data type
and provide some required helper functions:

```haskell
data Sing (t :: T) where
  STInt  :: Sing 'TInt
  STNat  :: Sing 'TNat
  STList :: Typeable t => Sing t -> Sing ('TList t)
  STPair :: (Typeable p, Typeable q)
         => Sing p -> Sing q -> Sing ('TPair p q)

fromSing :: Sing t -> T
fromSing = ...

data SomeSing where
  SomeSing :: Typeable t => Sing t -> SomeSing

withSomeSing
  :: SomeSing
  -> (forall t . Typeable t => Sing t -> a)
  -> a
withSomeSing (SomeSing a) f = f a

toSing :: T -> SomeSing
toSing = ...
```

Similarly to values, we'll defined typed representation of instruction.
Data type `Instr` is parametrized by type parameters `inp` and `out` of kind `[T]` which state
for input and output stack types corresponding to an instruction.
This representation of Michelson instructions is very elegant as it perfectly mimics notation
given in Michelson's documentation.

```haskell
data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Instr a b -> Instr b c -> Instr a c
  Nop :: Instr s s

  DROP :: Instr (a ': s) s
  DUP  :: Instr (a ': s) (a ': a ': s)
  SWAP :: Instr (a ': b ': s) (b ': a ': s)
  PUSH :: Val t -> Instr s (t ': s)

  PAIR :: Instr (a ': b ': s) ('TPair a b ': s)
  CAR :: Instr ('TPair a b ': s) (a ': s)
  CDR :: Instr ('TPair a b ': s) (b ': s)

  NIL  :: Instr s ('TList t ': s)
  CONS :: Instr (t ': 'TList t ': s) ('TList t ': s)

  ADDii :: Instr ('TInt ': 'TInt ': s) ('TInt ': s)
  ADDnn :: Instr ('TNat ': 'TNat ': s) ('TNat ': s)
  ADDin :: Instr ('TInt ': 'TNat ': s) ('TInt ': s)
  ADDni :: Instr ('TNat ': 'TInt ': s) ('TInt ': s)

  IF_CONS :: Instr (a ': 'TList a ': s) s'
          -> Instr s s'
          -> Instr ('TList a ': s) s'
```

Representation of `ADD` instruction is not very nice, but can be improved
with clever use of type class (explanation of this goes beyond the scope of this article).

Our function `typeCheckI` will take input stack type and an untyped instruction
and should return output stack type along with typed instruction.
Hence we introduce data types `Stack` and `SomeInstr`.
Data type `Stack` is similar to `Rec` from [vinyl package](http://hackage.haskell.org/package/vinyl)
with difference in that we impose `Typeable` constraint on the first argument of `:&`.

```haskell
data Stack inp where
  SNil  :: Stack '[]
  (::&) :: (Typeable s, Typeable a)
        => Sing a -> Stack s -> Stack (a ': s)
infixr 7 ::&

data SomeInstr inp where
  (:::) :: Typeable out
        => Instr inp out -> Stack out -> SomeInstr inp
infixr 5 :::
```

Now we're able to finally implement `typeCheck` function:

```haskell
typeCheckI
  :: Typeable inp => Stack inp -> UInstr -> Maybe (SomeInstr inp)
typeCheckI s UNOP = pure (Nop ::: s)
typeCheckI (_ ::& s) UDROP = pure (DROP ::: s)
typeCheckI (a ::& s) UDUP = pure (DUP ::: a ::& a ::& s)
typeCheckI (a ::& b ::& s) USWAP = pure (SWAP ::: b ::& a ::& s)
typeCheckI s (UPUSH t v) = withSomeSing (toSing t) $ \t' -> do
  val <- typeCheckVal t' v
  pure (PUSH val ::: t' ::& s)
typeCheckI (a ::& b ::& s) UPAIR = pure (PAIR ::: STPair a b ::& s)
typeCheckI (STPair a _ ::& s) UCAR = pure (CAR ::: a ::& s)
typeCheckI (STPair _ b ::& s) UCDR = pure (CDR ::: b ::& s)
typeCheckI (STInt ::& STInt ::& s) UADD = pure (ADDii ::: STInt ::& s)
typeCheckI (STNat ::& STNat ::& s) UADD = pure (ADDnn ::: STNat ::& s)
typeCheckI (STInt ::& STNat ::& s) UADD = pure (ADDin ::: STInt ::& s)
typeCheckI (STNat ::& STInt ::& s) UADD = pure (ADDni ::: STInt ::& s)
typeCheckI s (UNIL t) = withSomeSing (toSing t) $ \t' ->
  pure (NIL ::: STList t' ::& s)
typeCheckI ((_ :: Sing a) ::& STList (e :: Sing b) ::& s) UCONS = do
  Refl <- eqT @a @b
  pure (CONS ::: STList e ::& s)
typeCheckI (STList a ::& s) (UIF_CONS consCase nilCase) = do
  nc ::: (s' :: Stack out1) <- typeCheck s nilCase
  cc ::: (_ :: Stack out2) <- typeCheck (a ::& STList a ::& s) consCase
  Refl <- eqT @out1 @out2
  pure (IF_CONS cc nc ::: s')
typeCheckI _ _ = Nothing

typeCheck
  :: Typeable inp => Stack inp -> [UInstr] -> Maybe (SomeInstr inp)
typeCheck s [] = pure (Nop ::: s)
typeCheck s (i : []) = typeCheckI s i
typeCheck s (i : is) = do
  a ::: s' <- typeCheckI s i
  b ::: s'' <- typeCheck s' is
  pure (a `Seq` b ::: s'')
```

In `typeCheckI` we pattern-match on input stack type and an untyped instruction.
In case of `CONS` we need to additionally check equality of element on top of stack
and element type of list at the second element of stack.
In case of `IF_CONS` recursive call to `typeCheck` is used to check both continuations.

Now, when we finally settled down how to type check a sequence of
Michelson instructions, let's see how our eDSL can be interpreted.

```haskell
interpret
  :: Rec Val inp -> Instr inp out -> Rec Val out
interpret s Nop = s
interpret s (Seq a b) = interpret (interpret s a) b
interpret (_ :& s) DROP = s
interpret (a :& s) DUP = a :& a :& s
interpret (a :& b :& s) SWAP = b :& a :& s
interpret (a :& b :& s) PAIR = VPair a b :& s
interpret s (PUSH v) = v :& s
interpret (VPair a _ :& s) CAR = a :& s
interpret (VPair _ b :& s) CDR = b :& s
interpret (VInt a :& VInt b :& s) ADDii = VInt (a + b) :& s
interpret (VInt a :& VNat b :& s) ADDin = VInt (a + fromIntegral b) :& s
interpret (VNat a :& VInt b :& s) ADDni = VInt (fromIntegral a + b) :& s
interpret (VNat a :& VNat b :& s) ADDnn = VNat (a + b) :& s
interpret s NIL = VList [] :& s
interpret (a :& VList l :& s) CONS = VList (a : l) :& s
interpret (VList [] :& s) (IF_CONS _ nilCase) = interpret s nilCase
interpret (VList (a : l) :& s) (IF_CONS consCase _) =
  interpret (a :& VList l :& s) consCase
```

Interestingly, `interpret` function is total. This is a definite benefit of advanced type representation.
Data type `Val` contains enough information for type checker to consider all possible cases of input stack
and an instruction and we do not require to do some additional checks in runtime which may result in error.
In short, if the program type checks, it won't produce an error.

In this article we covered only a small subset of Michelson's instructions. Also, we consider only the core of Michelson's type system (without taking annotations into account). Clearing up all this details is a tedious work
we performed during work on [Morley framework](https://gitlab.com/morley-framework/morley/) and we welcome everybody to go and check the repository to see
the implementation of [type check](https://gitlab.com/morley-framework/morley/blob/master/src/Michelson/TypeCheck/Instr.hs) and [interpretation](https://gitlab.com/morley-framework/morley/blob/master/src/Michelson/Interpret.hs) with all underlying details.

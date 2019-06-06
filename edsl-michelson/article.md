# Parsing typed eDSL

Embedded DSL (or eDSL) is a popular technique for encoding your domain specific language into Haskell’s type system. One example of such DSL is Ivory – eDSL for C code generation. Even more often it’s useful to implement your DSL as a Haskell data type and interpret it right from Haskell.

Michelson is a smart contract language from the Tezos community. Akin to Forth, Michelson contract is described by a sequence of instructions operating on a typed stack. Each instruction assumes a stack of a certain type as input and produces an output stack of determined type. For example, the `PAIR` instruction presumes a stack of the type `a : b : s` and produces a stack of the type `pair a b : s` for any stack tail `s`.
You can read more about Michelson instructions and typing in the [official documentation](http://tezos.gitlab.io/zeronet/whitedoc/michelson.html).

In January 2019, in collaboration with Tocqueville Group of the Tezos foundation, Serokell started the [Morley project](https://gitlab.com/morley-framework/morley/).
One of its goals was to implement a comprehensive framework for testing arbitrary Michelson contracts that should support simple unit testing (when a contract is fed with particular sets of input and output values) as well as more complex property-based testing.

A small remark before we go forward.
In this article, we cover only a small subset of the Michelson's instructions and consider only the core of the Michelson's type system
without taking annotations into account.
Clearing up all these details was a complicated task
we performed during our work on the [Morley framework](https://gitlab.com/morley-framework/morley/), and we welcome everybody to go and check the repository to see
the implementation of a [type check](https://gitlab.com/morley-framework/morley/blob/b09fac13839f19056bf3799e25eb9c8f210999e1/src/Michelson/TypeCheck/Instr.hs#L178) and [interpretation](https://gitlab.com/morley-framework/morley/blob/b09fac13839f19056bf3799e25eb9c8f210999e1/src/Michelson/Interpret.hs#L299) with all underlying details.

It was decided to use Haskell for the Morley implementation, and as a first step we developed the Michelson language as a very simple AST data type:

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

Soon we understood this simple AST suffered from certain limitations. First of all, it was untrivial to generate arbitrary loosely-typed values. In our AST list was merely a constructor `UList [UVal]` and we couldn’t write an `Arbitrary` instance that would generate an arbitrary list of integers or strings depending on the type context.

The answer to this problem was obvious: create an AST with stronger types. Expression then becomes annotated with a type, which is easy to implement thanks to awesome `GADTs` and `DataKinds` extensions.
This immediately solves the problem of arbitrary value generation. And moreover, it becomes possible for the interpreter to stop unpredictably failing with runtime type errors.

```haskell
data Val t where
  VInt :: Int -> Val 'TInt
  VNat :: Word -> Val 'TNat
  VList :: [Val t] -> Val ('TList t)
  VPair :: Val p -> Val q -> Val ('TPair p q)
```

But after introducing this type, we quickly found ourselves at a challenge. It was very easy to parse textual code representation to a simple AST but was obscure how to do the same for a typed representation of Michelson. To simplify things, instead of parsing, we considered a task of conversion from a simple AST to a typed AST.

The first problem with conversion from a simple AST to a typed representation was that conversion of a parent branch in an AST depended on the types of children. A useful trick for solving this issue can be found in the [blog post from 2009](http://augustss.blogspot.com/2009/06/more-llvm-recently-someone-asked-me-on.html).
In short, we created an existential type holding value along with its type and returning this existential type from our type check function:

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

The `Sing` data type can be derived automatically with the use of the [singletons](http://hackage.haskell.org/package/singletons) library. That library provides the `Sing` data family and useful helper functions and classes for work with singletons.
Throughout this article, we'll stick to handwritten `Sing` and conversion functions.

There are two major problems with such construction.
First, a reader may have noticed that neither `STNat` nor `VNat` constructors were ever used.
Indeed, the `UInt` constructor from a simple AST was meant to represent both signed and unsigned integers because they roughly the same representation.
We can not really distinguish between `TInt` and `TNat` literals during parsing.

A similar issue appears in the case of a list constructor with an empty list wrapped in it.
When given a list constructor, we have no idea what type of values this list contains.
In the case of an empty list, we must return the `forall t. TList t` type, but our type representation does not support such construction.

The second problem with this snippet is similar.
In case of a non-empty list, we can take `t` as a type of the first element and figure out if other elements in
the list have the same type by comparing them. But to compare a `t1` type of the first list element with a `t2` type of the second list element, we need `Typeable t1`, `Typeable t2` constraints to hold.

It's relatively easy to address the second problem. We introduce a `SomeVal` data type with `Typeable` constraint
put in the scope of the constructor:

```haskell
data SomeVal where
 SomeVal :: Typeable t => Val t -> Sing t -> SomeVal
```

The first problem requires a switch to a different approach for type checking.
One way to solve the problem is to introduce some sort of constrained `forall` quantifier into
our simplistic type system, similar to what we have in Haskell.
For the empty list case, we can write something like `SomeVal (VList []) (forall n. Num n => n)`.
This approach is more universal but far heavier to implement and maintain.

Luckily for us, a lighter approach is possible for type checking Michelson programs.
Although all Michelson instructions are polymorphic, Michelson programs
are always given in the context of a contract.
A contract defines an input stack type, and each instruction modifies a stack type in an unambiguous way.
Hence, there is no actual need to implement a type checking algorithm that derives a type (with some `forall` quantifiers)
for an arbitrary sequence of Michelson instructions.
We are going to start with the input stack type and iterate through instructions.
This way we'll be able to stick to the type system represented by the `T` data type.

In this example, we considered only the type checking of values.
Following the rule defined above, we'll implement the `typeCheckVal` function with the first argument being a type
of an expression we're trying to parse. There are only two instructions that introduce new value to the stack
(namely, `UPUSH` and `UNIL`) and both of them explicitly have type representation included.

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

Note, that we do not actually need to wrap `Val t` into `SomeVal` here, because the first argument nicely defines the output of the function.
It's important to emphasise the role of singletons in the construction above. What we do is pattern-matching on the type
of value that should be parsed. Pattern-matching on type in the code of term-level functions is not common in Haskell, and singletons are perhaps the most straightforward way.

Now, let's implement a conversion for instructions. First, we'll have to modify our `Sing` data type slightly
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

Similarly to values, we'll define a typed representation of an instruction.
The `Instr` data type is parametrized by type parameters `inp` and `out` of kind `[T]` which state
for input and output stack types corresponding to an instruction.
This Michelson instructions representation is very elegant as it perfectly mimics the notation
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

The representation of the `ADD` instruction is not very nice but can be improved
with the use of type class.
We may create a type class `AddOp` which takes two type arguments
(for two operands of the `ADD` instruction). It will contain one function
for type checking, one function for interpretation and a type family for the
result type. For simplicity, this is not implemented in the article's code.

Our function `typeCheckI` will take an input stack type and an untyped instruction
and should return an output stack type along with a typed instruction.
Hence, we introduce `Stack` and `SomeInstr` data types.
The `Stack` data type is similar to `Rec` from the [vinyl package](http://hackage.haskell.org/package/vinyl)
with the difference in that, we impose `Typeable` constraint on the first argument of `:&`.

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

Now we're able to finally implement the `typeCheck` function:

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

In `typeCheckI` we pattern-match on an input stack type and an untyped instruction.
In the case of `CONS`, we need to additionally check the equality of an element on top of the stack
and an element type of the list at the second element of the stack.
In the case of `IF_CONS`, a recursive call to `typeCheck` is used to check both continuations.

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

Interestingly, the `interpret` function is total, which is a definite benefit of advanced type representation.
The `Val` data type contains enough information for a type checker to consider all possible cases of an input stack
and an instruction, and there's no need to perform additional checks in runtime, which is an error-prone practice.
In short, if the program type checks, it won't produce an error.

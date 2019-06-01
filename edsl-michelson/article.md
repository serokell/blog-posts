# Parsing typed eDSL

Embedded DSL (or eDSL) is a popular technique for encoding your domain specific language into Haskell’s type system. One example of such DSL is Ivory -- eDSL for C code generation. Even more often it’s useful to implement your DSL as Haskell data type and interpret it right from Haskell.

Michelson is a smart contract language from tezos community. Akin to Forth, Michelson contract is described by a sequence of instructions operating on a typed stack. Each instruction assumes stack of certain type as input and produces an output stack of determined type. For example, `PAIR` instruction presumes stack of type `a : b : s` and produces stack of type `pair a b : s` for any stack tail `s`. You can read more about Michelson instructions and typing in the official documentation.

In January 2019 in collaboration with Toquiville group of Tezos foundation Serokell started Morely project. One of Morley’s goals is to implement a comprehensive framework for testing arbitrary Michelson contracts. This testing should support simple unit testing (when contract is fed with particular sets of input and output values) as well as more complex property-based testing. It was decided to use Haskell for implementation of Morley and as a first step we implemented Michelson language as a very simple AST data type:

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
 | USOME
 | UNONE T
 | UIF_NONE [UInstr] [UInstr]
 | UPAIR
 | UCAR
 | UCDR
 | UADD
```

Soon we understood that this simple AST suffers from certain limitations. One issue was that it was untrivial to generate arbitrary loosely-typed values. In our AST list was merely a constructor `UList [UVal]` and we couldn’t write an `Arbitrary` instance that would generate an arbitrary list of integers or strings depending on type context.

An answer to this problem was obvious: create an AST with stronger types. Expression became annotated with a type, which is easy to implement thanks to awesome GADTs extension. This immediately solves the problem of arbitrary value generation. And moreover, it becomes possible for interpreter to stop unpredictably failing with runtime type errors.

```haskell
data Val t where
  VInt :: Int -> Val 'Tint
  VNat :: Word -> Val 'Tnat
  VOption :: Maybe (Val t) -> Val ('Toption t)
  VList :: [Val t] -> Val ('Tlist t)
```

But after introducing this type we quickly found ourselves at a challenge. It was very easy to parse textual code representation to simple AST, but is obscure how to do the same for the typed representation of Michelson. To simplify things instead of parsing we’ll consider a task of conversion from simple AST to typed AST.

First problem with conversion from simple AST to typed representation lies in the fact that conversion of parent branch in AST depends on the types of children. A useful trick for solving this issue can be found in this blog post from 2009. In short, we create an existential type which holds value along with its type and return this existential type from our type check function:

```haskell
data TWrapped (t :: T) where
 TwInt :: TWrapped 'TInt
 TwNat :: TWrapped 'TNat
 TwList :: TWrapped t -> TWrapped ('TList t)
 TwPair :: TWrapped p -> TWrapped q -> TWrapped ('TPair p q)

data SomeVal1 where
 SomeVal1 :: Val t -> TWrapped t -> SomeVal1

typeCheckVal1 :: UVal -> Maybe SomeVal1
typeCheckVal1 (UInt i) = Just $ SomeVal1 (VInt i) TwInt
typeCheckVal1 (UPair p q) = do
 SomeVal1 pv pt <- typeCheckVal1 p
 SomeVal1 qv qt <- typeCheckVal1 q
 pure $ SomeVal1 (VPair pv qv) (TwPair pt qt)
typeCheckVal1 _ = error "not implemented"
```

There are two major problems with such construction. First, a reader may have noticed that neither `TwNat` nor `VNat` constructors were ever used. Indeed, `UInt` constructor from simple AST was meant to represent both 



http://augustss.blogspot.com/2009/06/more-llvm-recently-someone-asked-me-on.html


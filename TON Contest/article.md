---
title: TON Blockchain Competition
author: kirelagin
date: 2019-10-23
keywords: [eDSL, Haskell, smart contracts, TON]
abstract: |
  We participated in the Blockchain Developer competition announced by the
  Telegram Open Network team.  Here is what we submitted as our entry and what
  we learned while working on it.
---


## Telegram Open Network

Telegram Open Network is a relatively new smart-contracts platform developed
by the team behind the Telegram messenger. It was announced in late 2017 and
first source code was published in September this year. Three (TODO update) weeks
ago, they started a [competition.][contest-announce] In it, developers were asked
to either implement a smart-contract or contribute to the platform in one way or
another.

After giving it a little consideration, we decided to participate as a company and
implement two smart-contracts from the list of organizer suggestions.
For one of them we chose to use the development tools provided
with the TON distribution, for the other one we decided to do what we like doing
the most: implement it in a new language built specifically for TON and embedded
into Haskell with its incredibly rich type system.

We believe the plan worked out exceptionally well, so we would like to showcase
our entries and our approach to smart contracts and eDSLs.
The competition was incredibly fun and engaging and, hopefully, so will be
this blog post. Let’s dive right in.

### TON Blockchain Research

We always try to keep on top of recent developments in the areas that we work in,
blockchain being one of them, thus we were already familiar with the ideas
from the [TON white paper][ton:whitepaper]. However, we hadn’t looked at the
_technical_ documentation and the actual source code of the platform before, so
this was an obvious first step. If you are interested too, you can find the
official documentation at <https://test.ton.org> and in the [source repository][repo:doc].

Since the code had been published for quite a while by that time, we also tried searching
for guides or summaries produced by _users_, but, unfortunately, all we could find
were tutorials showing how to build the platform on Ubuntu, which was not relevant
to us anyway (you will see why in a minute).

The documentation turned out to be very thorough but sometimes hard to read as it
was constantly jumping back and forth between high level explanations of abstract
ideas and low level details of their specific implementations.

### Nix: Building the code

Here at Serokell we are huge fans of [Nix]: all our servers run on [NixOS],
we build our projects with [Nix] and deploy using [NixOps]. It helps us make sure
are builds are reproducible and will work on any OS supported by Nix without us
worrying about OS or distribution specific aspects of the build.

Therefore, we started by creating a [Nix overlay with a build expression for TON][ton.nix].
You can find more details if you follow the link, but, long story short, with this
overlay compiling TON is as simple as:

```shell
$ cd ~/.config/nixpkgs/overlays && git clone https://github.com/serokell/ton.nix
$ cd /path/to/ton/repo && nix-shell
[nix-shell]$ cmakeConfigurePhase && make
```

Note that you don’t have to worry about installing the build dependencies,
Nix will magically handle everything for you whether you are running NixOS,
Ubuntu, or macOS on a MacBook.

Everyone should be using Nix for all their building needs!

### Programming for TON

The code of smart-contracts existing in the TON Network is executed by a virtual
machine called TON Virtual Machine (TVM). More complex than most virtual
machines, this one provides some quite unconvential capabilities, such as
native support for continuations and data references.

They created three (!) new programming languages:

* _Fift_ is a general-purpose stack-based programming language, somewhat
  similar to [Forth][wiki:forth]. Its special power is the built-in support
  for interfacing with the TON Virtual Machine (TVM) that runs smart-contracts
  in the TON Network.
* _Func_ is a smart-contract programming language that feels a lot like [C][wiki:c]
  and compiles to yet another language called Fift Assembler.
* _Fift Assembler_ is a little different from “traditional” programming languages
  in that it doesn’t have a compiler. Instead, it is an
  [Embedded Domain-Specific Language (eDSL)][wiki:edsl], in other words, it is
  a Forth _library_ that allows one to write Forth programs that generate binary
  executable code for the TVM.


## Our competition entries

### Asynchronous payment channel

A “payment channel” is a smart-contract that allows two users to send payments
to each other off-chain, thus saving money (transaction fees) and time
(you don’t have to wait for a block to be issued). This way, the payments can
be as small and frequent as needed, and the users still do not need to trust
each other, as the final settlement is _guaranteed_ by the smart contract.

After thinking about it for a couple of times, we realised that there was a
pretty simple solution to the problem: the two parties can exchange signed
messages where each message will, essentially, carry two numbers: the
_total_ amounts paid by each of them so far. These two numbers will work
as [Vector clock][wiki:vectorc] in traditional distributed systems and thus
will induce a “happened-before“ order on the payment transactions, which will
allow the contract to resolve any possible conflicts in the end. We played
a little with the idea and realised that just one number is enough, however
we still decided to keep both for UX purposes; we also decided to include
the payment amount in every message merely for UX as well. Without it, if
some of the messages get lost on their way, while the total sums and thus
the final settlement will be correct, the users wouldn’t notice that something
was lost.

In order to verify our idea, we did a quick search. To our great surprise, we
did not find a lot of mentions of this simple and elegant payment channel protocol.
In fact, we found only two:

1. [this explanation][medium:paychan] of essentially the same idea but only for
   the case of a uni-directional channel;
2. [this tutorial][ptb:paychan] that presents the same idea as ours, but does not
   go into a lot of detail regarding correctness and dispute resolution.

Somewhat puzzled we drafted our own specification of this protocol, trying to make
it very detailed and focusing on the explanation of its correctness.
After a couple of iterations it was ready and you are welcome to
[have a look at it][spec]. With the specification at hand, we set off to
write the code.

We implemented the contract in Func and, following the recommendations of the
organisers, the command-line tool for interacting with our contract was entirely
in Fift. We could have chosen any other language for our CLI, but we thought
it would be interesting to try Fift and see how it works for us in this case.

In retrospect, we can say that we don’t really see any good reasons to prefer
Fift to other well-established and supported languages with good libraries
and tooling. Programming in a stack-based language is unnecessarily hard and
is especially bad due to Fift’s lack of static types – keeping track of your
stack layout requires a lot of effort.

Because of the above, the only justification for the existence of Fift seems to
be its role as a host language for Fift Assemebler. “But wouldn’t it be a better
idea to embed the TVM Aseembler into some other language, instead of inventing
a new one for this sole purpose?” – you might wonder. Well, we’re glad you asked!

### TVM Haskell eDSL

We also decided to implement a multisignature wallet, but we thought that writing
another Func contract would be not that interesting, so we added a twist: our
own assembler language for TVM. Just as Fift Assembler, our
new language was embedded into another language but
we chose Haskell as the host. This gave us access to all the power of Haskell’s
static types, and we are firm believers of static typing, especially when working
with smart-contracts – an area where the cost of a small mistake can be very high.

To give you an idea of what TVM assembler embedded into Haskell feels like, we
have reimplemented the standard wallet contract in it. Before you take a look at
the code, here are a couple of important things to keep in mind:

* This contract consists of a single function, but you can have many of them.
  When you define a new function in the host language (that is Haskell), our
  eDSL allows you to choose whether you want it to be translated into a separate
  routine in TVM or inlined at the place of the call.
* Just like in Haskell, functions have their types specified and these are checked
  during compilation. In our eDSL, the input type of a function is the type of the
  stack that it expects, and the output type is the type of the stack that its
  invocation will result in.
* There are `stacktype` annotations here and there in the code. In the original
  wallet contract these were just comments, but in our eDSL they are actually
  part of the code and are checked at _compile time_. These can serve as documentation
  and as assertions that can help the developer find an issue in case they make
  a change in the code and something doesn’t compile. Of course, they do not have
  any effect on performance at _run time_ as they do not result in any TVM code
  being generated.
* It is still a prototype quickly put together in about two weeks. There is plenty
  of room for improvement, for example all class instances you will see in the code
  below can (and should) be auto-generated.

And now, here is a full reimplementation of the simple wallet in our eDSL:

```haskell
main :: IO ()
main = putText $ pretty $ declProgram procedures methods
  where
    procedures =
      [ ("recv_external", decl recvExternal)
      , ("recv_internal", decl recvInternal)
      ]
    methods =
      [ ("seqno", declMethod getSeqno)
      ]


data Storage = Storage
  { sCnt :: Word32
  , sPubKey :: PublicKey
  }

instance DecodeSlice Storage where
  type DecodeSliceFields Storage = [PublicKey, Word32]
  decodeFromSliceImpl = do
    decodeFromSliceImpl @Word32
    decodeFromSliceImpl @PublicKey

instance EncodeBuilder Storage where
  encodeToBuilder = do
    encodeToBuilder @Word32
    encodeToBuilder @PublicKey

data WalletError
  = SeqNoMismatch
  | SignatureMismatch
  deriving (Eq, Ord, Show, Generic)

instance Exception WalletError

instance Enum WalletError where
  toEnum 33 = SeqNoMismatch
  toEnum 34 = SignatureMismatch
  toEnum _ = error "Uknown MultiSigError id"

  fromEnum SeqNoMismatch = 33
  fromEnum SignatureMismatch = 34

recvInternal :: '[Slice] :-> '[]
recvInternal = drop

recvExternal :: '[Slice] :-> '[]
recvExternal = do
  decodeFromSlice @Signature
  dup
  preloadFromSlice @Word32
  stacktype @[Word32, Slice, Signature]
  -- cnt cs sign

  pushRoot
  decodeFromCell @Storage
  stacktype @[PublicKey, Word32, Word32, Slice, Signature]
  -- pk cnt' cnt cs sign

  xcpu @1 @2
  stacktype @[Word32, Word32, PublicKey, Word32, Slice, Signature]
  -- cnt cnt' pk cnt cs sign

  equalInt >> throwIfNot SeqNoMismatch

  push @2
  sliceHash
  stacktype @[Hash Slice, PublicKey, Word32, Slice, Signature]
  -- hash pk cnt cs sign

  xc2pu @0 @4 @4
  stacktype @[PublicKey, Signature, Hash Slice, Word32, Slice, PublicKey]
  -- pubk sign hash cnt cs pubk

  chkSignU
  stacktype @[Bool, Word32, Slice, PublicKey]
  -- ? cnt cs pubk

  throwIfNot SignatureMismatch
  accept

  swap
  decodeFromSlice @Word32
  nip

  dup
  srefs @Word8

  pushInt 0
  if IsEq
  then ignore
  else do
    decodeFromSlice @Word8
    decodeFromSlice @(Cell MessageObject)
    stacktype @[Slice, Cell MessageObject, Word8, Word32, PublicKey]
    xchg @2
    sendRawMsg
    stacktype @[Slice, Word32, PublicKey]

  endS
  inc

  encodeToCell @Storage
  popRoot


getSeqno :: '[] :-> '[Word32]
getSeqno = do
  pushRoot
  cToS
  preloadFromSlice @Word32
```

You can see the full source code of our eDSL and the multisig contract in
[this repository][repo:fift-asm-dsl]. If you got interested in typed
eDSLs, you will most certainly like [this blog post][blog:edsl] by one of
my brilliant colleagues that goes into way greater depths than I ever could.

## Our thoughts on the competition & TON

First of all, we enjoyed the competition a lot! It gave us an unexpected break
from our daily responsibilities (not that we don’t enjoy doing what we do
daily!). This spirit of a hackathon, close team work, the need to quickly
dive into a new technology – I think all engineers know how exciting it is.

We were impressed by the amount of work done by the TON team. They managed
to build a pretty complex and at the same time beautiful system. And, most
importantly, it works! However, we are not convinced _all_ of this work
was strictly necessary. Being engineers, we can definitely relate
to the idea of Fift, a brand-new stack based (that is, in some sense, somewhat
esoteric) programming language, but we believe that real-world applications
more complex than simple CLI _prototypes_ are beyond its capabilities, and
what became _Fift_ Assembler could have as easily been embedded into some
other language (like Haskell!).

The same can be said about Func. Implementing a new high-level language from
the ground up (they even have their own parser!) is certainly _fun_,
but we can’t really _see_ the need for it. As a short-term strategy,
the team could have taken an existing smart-contract language and adapted
it to emit code for TVM; while in the long run we fell that having an
[LLVM][wiki:llvm] backend for TVM would be great, as it would allow for
a wide variety of source languages.

The above is especially true, given that it is clear that TVM has been designed
with very high-level source languages (Haskell!) in mind. This makes us think
that Func is not meant to be used for actual production code, but is merely a
demo, a prototype of a TVM-compatible high-level programming language, and
if this is the case, then it does not make sense to put a lot of effort into it.

Overall, TON feels like a great platform and it surely has potential. There
is a lot to be done to make the TON ecosystem truly flourish, both in terms
of using it to implement solutions that require a blockchain infrastructure,
and improving the tooling used to implement such solutions, and we would be
proud to be part of this endeavour. So, if you think about relying on TON
to solve your problem, [let us know](mailto:hi@serokell.io), we will be
able to help.


[contest-announce]: https://t.me/contest/102
[ton:whitepaper]: https://test.ton.org/ton.pdf
[repo:doc]: https://github.com/ton-blockchain/ton/tree/master/doc

[nixos]: https://nixos.org/
[nix]: https://nixos.org/nix
[nixops]: https://nixos.org/nixops
[ton.nix]: https://github.com/serokell/ton.nix

[medium:paychan]: https://medium.com/@matthewdif/ethereum-payment-channel-in-50-lines-of-code-a94fad2704bc
[ptb:paychan]: https://programtheblockchain.com/posts/2018/02/23/writing-a-simple-payment-channel/

[wiki:vectorc]: https://en.wikipedia.org/wiki/Vector_clock
[wiki:forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)
[wiki:c]: https://en.wikipedia.org/wiki/C_(programming_language)
[wiki:edsl]: https://en.wikipedia.org/wiki/Domain-specific_language
[wiki:llvm]: https://en.wikipedia.org/wiki/LLVM

[spec]: https://github.com/serokell/ton-paychan/tree/master/doc/Payment-channel.md
[repo:fift-asm-dsl]: https://github.com/serokell/fift-asm-dsl
[blog:edsl]: https://serokell.io/blog/parsing-typed-edsl

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
by the team behind the Telegram messenger. It was announced in (TODO year) and
the first source code was published on (TODO date). Three (TODO update) weeks
ago they started a [competition for developers][contest-announce], who were asked
to either implement a smart-contract or contribute to the platform in one way or
another.

After giving it a little consideration we decided to participate as a company.
We decided to implement two smart-contracts from the list that the organisers
suggested and while for one of them we chose to use the development tools provided
with the TON distribution, for the other one we decided to do what we like doing
the most: implement it in a new language built specifically for TON and embedded
into Haskell with its incredibly rich type system.

Let us say right away that we believe the plan worked out exceptionally well,
so we would like to show you (TODO blabla we want to share what we did and
tell everyone how amazing it is and why it is so amazing and explain why we
did it this way).

### Research

We always try to keep on top of recent developments in the areas that we work in,
blockchain being one of them, thus we were already familiar with the ideas
from the [TON white paper][ton:whitepaper]. However, we haven’t looked at the
_technical_ documentation and the actual source code of the platform before, so
this was an obvious first step. If you are interested too, you can find the
official documentation at <https://test.ton.org> and in the [source repository][repo:doc].

Since the code had been published for quite a while by that time, we also tried searching
for guides or summaries produced by _users_, but, unfortunately, all we could find
were tutorials showing how to build the platform  on Ubuntu, which was not relevant
to us anyway (you will see why in a minute).

The documentation turned out to be very thorough but sometimes hard to read as it
is constantly jumping back and forth between high level explanations of abstract
ideas and the low level details of their specific implementation.

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


## Our competition entries

### Asynchronous payment channel

A “payment channel” is a smart-contract that allows two users to send payments
to each other off-chain, thus saving on the transaction costs and not having
to wait for a new block being issued every time. This way the payments can
be as small and as frequent as needed, and the users still do not need to trust
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
the payment amount in every message merely for UX as well: without it if
some of the messages get lost on their way, while the total sums and thus
the final settlement will be correct, the users wouldn’t notice that something
was lost.

In order to verify our idea we did a quick search and to our great surprise
did not find this simple and elegant payment channel protocol documented
anywhere. The closest thing is [this explanation][medium:paychan] of
essentially the same idea but only for the case of a uni-directional channel.
Somewhat puzzled we drafted our own specification of this protocol and
after a couple of iterations it was ready and you are welcome to
[have a look at it][spec]. With the specification at hand we set off to
write the code.

For this contract we picked _Fift_ and _Func_ as our tools of choice. These
are two programming languages created by the TON team specifically for their
blockchain platform:

* _Fift_ is a general-purpose stack-based programming language, somewhat
  similar to [Forth][wiki:forth]. Its special power is the built-in support
  for interfacing with the TON Virtual Machine (TVM) that runs smart-contracts
  in the TON Network.
* _Func_ is a smart-contract programming language that feels a lot like [C][wiki:c]
  and compiles to yet another language called Fift Assembler.
* _Fift Assembler_ is a little different from “traditional” programming languages
  in that it doesn’t have a compiler. Instead, it is an
  [Embedded Domain-Specific Language (EDSL)][wiki:edsl], in other words, it is
  a Forth _library_ that allows one to write Forth programs that generate binary
  executable code for the TVM.

We implemented the contract in Func and, following the recommendations of the
organisers, the command-line tool for interacting with our contract was entirely
in Fift. We could have chosen any other language for our CLI, but we thought
it would be interesting to try Fift and see how it works for us in this case.

In retrospect we can say that we don’t really see any good reasons to prefer
Fift to other well-established and supported languages with good libraries
and tooling. Programming in a stack-based language is unnecessarily hard and
is especially bad due to Fift’s lack of static types – keeping track of your
stack layout requires a lot of effort.

Because of the above, the only justification for the existence of Fift seems to
be its role as a host language for Fift Assemebler. “But wouldn’t it be a better
idea to embed the TVM Aseembler into some other language, instead of inventing
a new one for this sole purpose?” – you might wonder. Well, we are glad you asked!

### TVM Haskell EDSL





[contest-announce]: https://t.me/contest/102
[ton:whitepaper]: https://test.ton.org/ton.pdf
[repo:doc]: https://github.com/ton-blockchain/ton/tree/master/doc

[nixos]: https://nixos.org/
[nix]: https://nixos.org/nix
[nixops]: https://nixos.org/nixops
[ton.nix]: https://github.com/serokell/ton.nix

[medium:paychan]: https://TODO

[wiki:vectorc]: https://en.wikipedia.org/TODO
[wiki:forth]: https://en.wikipedia.org/TODO
[wiki:c]: https://en.wikipedia.org/TODO
[wiki:edsl]: https://en.wikipedia.org/TODO

[spec]: https://github.com/serokell/ton-paychan/tree/master/doc/Payment-channel.md

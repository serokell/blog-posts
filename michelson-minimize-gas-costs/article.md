# Minimizing transaction costs of Tezos smart contracts

This post is for developers that are interested in the Tezos platform in general and its smart contract language — Michelson — in particular.
It should be useful for people developing Michelson smart contracts and high-level languages compiling to Michelson.

## Introduction

Michelson is the smart contract language of [Tezos](https://tezos.com/).
Serokell team has been working on tooling around Tezos and Michelson for 1.5 years.
Over time, we have implemented several Tezos smart contracts, and some of them are already in production.
We had to optimize our smart contracts numerous times, and we want to share some guidelines and approaches we developed over time.

DISCLAIMER: Tezos is an upgradeable blockchain, and its protocol can be changed over time.
All the statements in this article are valid for the current Tezos protocol — Carthage.

## The problem

In this article, we focus on the problem of making Michelson smart contracts smaller and more efficient.
There are three primary reasons why it's necessary.
The first two are more important for contract developers while the third one affects contract users (but still should be handled by developers).

1. Tezos limits the amount of gas one operation can consume.
In Carthage protocol, one operation can consume up to 1040000 gas.
If an operation emits internal operations (e. g. other smart contract calls), total gas consumption is limited by this value.
So, if you implement a smart contract such that operations involving it consume more than a certain amount of gas, it will be impossible to use your contract.
Moreover, there is a gas limit per block that is 10 times greater than the per-operation limit.

2. In addition to the gas limit, there is another hard limit: each serialized operation can have at most 16384 bytes.
To use a smart contract you need to originate it — "upload" to the blockchain.
You can do it by submitting an origination operation which stores, among other things, the whole smart contract code.
It means that if your contract is more than about 16 kilobytes (16384 bytes minus initial storage size and some other data), you can't even originate it.

3. Transaction costs in Tezos depend on gas consumption and storage usage.
When you make a transaction, you pay two costs:
  + Fee that goes to the baker.
    It depends on the amount of gas consumed by your transaction.
  + Storage cost that gets burned.
    If your transaction increases the amount of data permanently stored in the blockchain, you have to pay for that.
    Storage cost is burned, i. e. nobody receives what you pay.

A consequence of the point (3) is that the bigger your serialized contract is, the more you pay to deploy it.
You can read more about transaction costs [here](https://gitlab.com/morley-framework/morley/-/blob/1f4ad392173a49752f1326a9dd4a4d5b7f6c5e70/docs/fees.md).

Let's formalize the problem a bit.
In our case, there are two functions:
1. `size(contract)`.
2. `gas(contract)`.

The first function is fairly simple: it returns the size of the serialized (into binary representation) contract.

The second function is more complex: normally, there is no single "gas" value we can statically compute for a contract.
When you originate a contract, gas consumption depends on the initial storage.
When you make a transfer to a contract, gas consumption depends on the parameter.
For this reason, we define `gas` function to be a vector, not a single number: `gas(contract) = (g₀, g₁, … gₙ)`.
`g₀` is gas consumption for origination, `g₁` is gas consumption of calling the first entrypoint, `g₂` is gas consumption of the second entrypoint, etc.
`g₀` still depends on the initial storage value, and each other `gᵢ` depends on the current storage and parameter.
So we, in turn, define `gᵢ` as a pair consisting of average gas consumption and worst-case gas consumption, i. e. two numbers.
This article is not intended to be math-heavy, so we omit the formal definitions of "average" and "worst-case" gas consumption.
Hopefully, these words are sufficiently intuitive for semi-formal understanding.
Note that if a smart contract is intended to be called by another smart contract, each `gᵢ` value includes gas from all calls.

When you develop a smart contract, you must ensure that:
1. `size(contract) ≤ 16384 - is - c` where `is` is the size of initial storage and `c` is the size of everything else that constitutes the origination operation. We assume that both `is` and `c` are constant.
2. For each `i`, `gᵢ.worst` is at most 1040000.

Apart from that it's desirable to minimize `size` and all `gᵢ` values.
Even though it's desirable, it's not so critical.
* By minimizing the `size`, you decrease the cost of the contract origination (which is payed once).
* By miniziming the `gas`, you decrease the cost of each transaction by decreasing the fees.
Additionally, you increase the number of transactions that can be put into one block.

Increasing the number of transactions in one block can be useful, for example, when you want to make operations that work on large lists.
Let's say your entrypoint takes a list of size up to 100 but the user of your contract wants to process 10000 items.
They have to make 100 transactions.
The more they put into one block, the fewer blocks will be necessary and the faster it will work.

To give you an idea on how large "1040000 gas" and "16384 bytes" values are, let me write a bit about our smart contracts.

### How large are these limits?

Michelson is a low-level language, and programming in it directly gets complicated once your contract has more than ≈100 instructions.
There are a plenty of higher-level languages that compile to Michelson.
As with mainstream programming languages, the choice of the language affects efficiency of the generated code.
High-level languages simplify development effort, but are likely to produce less optimal code than you could write in a low-level language.

We have developed the [Lorentz eDSL](https://gitlab.com/morley-framework/morley/-/tree/675128486680812a4205719934e3dd8ba121210a/code/lorentz) for Michelson programming.
Most of our smart contracts are implemented using Lorentz.
Lorentz offers various features not available in vanilla Michelson, but is still quite low-level.
In particular, there are no variables and you have to work with the stack directly.
It means that for contracts written in Lorentz, the overhead of programming in a high-level language should be quite small.

Most of the smart contracts we have developed are token smart contracts that follow the [FA1.2](https://gitlab.com/tzip/tzip/-/blob/b916f32718234b7c4016f46e00327d66702511a2/proposals/tzip-7/tzip-7.md) specification.
`FA1.2` is a Tezos version of the [ERC-20](https://eips.ethereum.org/EIPS/eip-20) token standard which is popular in the Ethereum world.
Each our contract contains all methods listed in FA1.2 and some other methods specific to that particular contract.
On average, I think, our contracts consist of:
1. FA1.2 logic (`transfer`, `approve` and getters) + manager operations (`mint`, `burn`, pausing).
2. About the same amount of custom, application-specific logic.

Almost all our contracts had issues with operation size or gas consumption at some point.
However, for most of them, it was not too hard to resolve all these issues.
Some smart contracts were developed before the Carthage protocol update when the gas limit was 800000, so some issues were partially solved by this upgrade.

To sum up: if you asked me how large contracts in Tezos can be, I would say that current limits of Tezos permit writing smart contracts that:
1. Have all FA1.2 (or ERC-20) logic.
2. Have additional "manager" token operations such as `mint` and `burn`.
3. Have about the same amount of custom logic as items (1) and (2) combined.
4. Are implemented in a language that doesn't add too much overhead compared to writing optimal code in low-level Michelson.

Of course, the contract does not have to implement token functionality: it can have any other logic.
Here, I am just trying to estimate how much logic one can put into a smart contract using token functionality as an example.

Note that sometimes one may want to call one contract from another contract, e. g. a multisig contract.
In this case, total gas spent by all contract calls should fit into the limit.

## Solutions

In this chapter, we provide some hints that can help Michelson developers write more optimal smart contracts.

### Be aware of the Tezos gas model

Before making any optimizations, it's very important to know how the function you are trying to optimize is computed.
In our case, we want to optimize two functions mentioned above: `size(contract)` and `gas(contract)`.

To a first approximation, we can assume that `size(contract) = α · n + s` where `α` is some constant value, `n` is the number of instructions and `s` is the sum of the sizes of all variable-size constants (strings, bytes) in the contract.
So, the `size` optimization boils down to decreasing the number of instructions and long constants.

Gas consumption (i. e. the `gas` function) is more complicated.
We have thoroughly researched the Tezos gas model.
A detailed description can be found [here](https://gitlab.com/morley-framework/morley/-/blob/1f4ad392173a49752f1326a9dd4a4d5b7f6c5e70/docs/gasConsumption.md).
**The most important observation** is that normally majority of gas is spent on conversion of byte sequences into a protocol-specific _typed_ representation and back.
Intuitively, it means reading, deserialization and parsing of bytes, checking correctness of types, and conversion back to bytes.

Apart from that, there is interpreter gas consumption: gas that is spent to actually interpret instructions.
Interpreter gas consumption is significantly lower than the cost of other gas consuming operations.
So if you call a smart contract that does `UNPAIR; ADD; NIL operation; PAIR`, most of the gas will be spent on turning this code to the typed representation and then putting the update storage into the database.
A small amount of gas will be spent on the actual stack manipulation and addition.
However, there are some instructions that can be very expensive: `CONTRACT`, `PACK`, `UNPACK`, `big_map` access (`GET` and `MEM`).
Please read the aforementioned document for more detail.

Since interpreter gas consumption for most operations is much lower than other gas costs, we can approximate gas consumption as `β · n + s + i + ε`. Specifically:
1. Gas consumption other than interpretation is mostly static and we assume that it linearly depends on the contract size that we have previously defined as `α · n + s`.
The coefficient can be different here, hence `β · n + s`.
Of course, it's not exactly correct, but it's a good approximation.
2. `i` is the cost of interpretation of expensive operations: `CONTRACT`, `PACK` and others.
Usually, there are not many of them and each can be considered separately.
3. `ε` is the cost of interpretation of other operations, along with all the unaccounted gas.
We assume it to be much smaller than everything else.

That being said, we are primarily interested in minimizing three values:
1. The total number of instructions (`n`).
2. The total size of variable-length constants (mostly strings).
3. The interpretation cost of expensive instructions:
  + The `CONTRACT` instruction is expensive on its own (regardless of the address you apply it to).
  It has a constant cost of 10000 gas that's spent even if you apply it to an implicit account (i. e. an address that starts with `tz`).
  If you apply it to a large contract, it will be very expensive.
  Note that the whole contract is stored as a bytestring, so in order to check its parameter type, the whole contract is read and deserialized.
  + `PACK` and `UNPACK` instructions applied to large values.
  + `big_map` reads, especially if you read a large value.

If your contract is expensive to call (in terms of gas), usually you shouldn't prioritize decreasing the number of steps necessary to **interpret** it, for example by moving some code outside of `ITER`.
It may save you some gas, but you will most likely save more gas by making your contract smaller.

### Avoid inter-contract communication

In Tezos, one contract can call another contract using the `TRANSFER_TOKENS` instruction.
It adds a transfer operation to the queue of internal operations when the interpretation of the current contract ends.
When you develop a large smart contract, you may want to split it into multiple smart contracts to achieve better modularity.
In some cases, it can be quite expensive, though.
Imagine that you have two smart contracts: `A` and `B`.
`A` has a `foo` entrypoint that calls `B`'s `bar` entrypoint which in turn calls `A`'s `baz`.
In this case:
1. When `A.foo` is called, the whole `A` contract will be read and turned into the typed representation.
2. When `A.foo` calls `B.bar`, the whole `B` contract will be read and turned into the typed representation.
3. When `B.bar` calls `A.baz`, the whole `A` contract will be read and turned into the typed representation once again!
4. Conversion from the typed representation to the binary form and writing to the database will happen 3 times as well (to update the storage).

Since interpretation gas consumption is relatively small, we can say that calling `A.foo` and `A.bar` consumes about the same amount of gas.
So in this scenario, the execution of `A` consumes twice the gas, and some gas is consumed by `B`.
Moreover, there is an overhead on using the `TRANSFER_TOKENS` instruction: the `CONTRACT` instruction itself costs 10000 gas (not considering additional costs).
So, if you make 2 calls using `TRANSFER_TOKENS` (even if you only recursively call the same contract twice), the average limit for one contract decreases from 1040k to approximately 300k-340k.
Of course, in this case some logic is moved to `B`, so `A` and `B` are both smaller than `A + B`.
If `A` and `B` have approximately equal size, the limit will be about 300k for `(A + B)/2` which is still significantly less than 1040k for the whole `A + B`.

In theory, inter-contract communication could be much less expensive if:
1. It was possible to load (i. e. read, deserialize, parse and compare types) only a single entrypoint. In our contracts, we usually have 10+ entrypoints, so processing the whole contract is much more expensive than doing it with per-entrypoint granularity.
2. After a contract (or its entrypoint) is loaded into the typed representation, further operations involving this contract didn't spend gas on loading it again.

We hope that this situation will be improved in a future Tezos update.

If there are no recursive calls (maybe indirectly recursive), there is still some overhead.
For example, let's imagine that we split a smart contract `C` into `A` and `B`.
`A` is the main contract, and it calls `B`.
In this case, `size(C) ≈ size(A) + size(B)`, and one may think that `gas(C) ≈ gas(A) + gas(B)`, so this split is not a problem.
However, in this case:
1. You need to use the `CONTRACT` instruction that has a fixed cost of 10k gas and some extra costs.
2. You need to pass some data that is directly available in `A` to `B`.
Most likely, you will need to add some instructions that wrap this data into `B`'s parameter.
And you need the `TRANSFER_TOKENS` instruction.
And then in `B`, you might need to unwrap this data back.

In a simple scenario when you have only two smart contracts and only one inter-contract call, the overhead can be small and not problematic.
But it's still not free.
You will likely have substantial problems in one of these cases:
1. There is a (potentially indirect) recursion, i. e. some contract is called more than once.
2. There are many contracts involved, even if each of them is called only once.
Overhead of a single call is not that big, but when there are more, it gets bigger.

The contract calling mechanism is different from typical function calls that many programmers are used to.
It gets even more complicated when you want to get a value computed by another contract.
Basically, you have to call another contract and then have it call your contract back.
It's easy to make a mistake and introduce a vulnerability when you implement such interaction.
Since this article is about minimizing gas consumption and contract size, we will not elaborate on this point but that's an additional reason why we prefer to avoid inter-contract communication in our smart contracts.

Spliting your smart contract's logic into multiple smart contracts can help with the operation size issue.
If your contract origination exceeds the 16384 bytes limit, you may be tempted to split your contract into smaller parts (contracts).
However, most likely it's more efficient to put some code into `big_map`s and load it lazily.
The next section describes this in more detail.

### Lazy processing of code

Tezos operates with full smart contracts only.
Before the Babylon (005) update, there was no notion of "entrypoint".
You could only pass a value of the whole parameter type to a contract.
The addition of entrypoints made smart contracts more granular for their users, but internally each smart contract is still one inextricable unit.
If you call an entrypoint called `foo`, Tezos will automatically construct a value of a large type (a tree of `or`s) and execute the whole code of your smart contract (that typically has a bunch of `IF_LEFT` instructions around the beginning).
If you have `n` entrypoints of approximately equal size, Tezos will load up to `n` times more code than will be interpreted.
That might be quite inefficient for large `n`.

In this case, one way to decrease gas consumption is to split your smart contract into smaller parts and only load the parts necessary for interpretation.
The idea is to put smart contract code into `big_map`s as `lambda`s.
Then, the storage will contain:
1. `storage₀` which is the original storage type before applying this optimization.
2. `big_map bool (lambda (pair argᵢ storage₀) (pair (list operation) storage₀))` for each entrypoint with argument `argᵢ`.

For example, imagine that your contract has two entrypoints — `inc` and `reset` — and stores one `int` in its storage.
`inc` has a `nat` argument, `reset` has a `unit` argument.
We consider only 2 entrypoints and 1 storage field for simplicity. For such a small contract this optimization will likely be useless, but it should demonstrate the idea. In this case, the storage will have:
1. The original `int`.
2. `big_map bool (lambda (pair nat int) (pair (list operation) int))`
3. `big_map bool (lambda (pair unit int) (pair (list operation) int))`

During origination, you provide `{ Elt True inc_lambda }` as the first `big_map` and `{ Elt True reset_lambda }` as the second one.
Here `inc_lambda` and `reset_lambda` implement the logic originally present directly in the `inc` and `reset` entrypoints, respectively.
Note that there is only one possible value for a key, so it would be better to use the `unit` type, but unfortunately it is not comparable and can not be used as a key in `big_map`.
You can replace `True` with `False` as long as it is used consistently.

The implementation of the `inc` entrypoint (as present in the final contract, not `inc_lambda`) should get the first `big_map` from the storage, `GET` a lambda from it passing `True` as a key, assert that `SOME` value was returned, `EXEC` the lambda, and wrap its result according to the calling convention (basically wrap `storage₀` into the actual storage).

Note that this lazy storage infrastructure adds some overhead since you need to make extra `GET`, `EXEC`, wrapping and unwrapping.
It should be useful if two assumptions hold:
1. Each entrypoint is smaller than some big part of the contract, let's say half of the contract.
2. The contract itself is relatively large.

Let's say the fixed cost of a contract is about 400k gas and the largest entrypoint contributes about 200k to it.
If you put all entrypoints into the storage, the most expensive entrypoint will cost `200k + lazy loading infra cost`.
Lazy loading infra should cost much less than 200k gas.
That being said, the efficiency of this optimization depends on the contract you are applying it to, so don't apply it blindly.

Of course it's not necesary to put all entrypoints into the storage, you can apply this approach only to some parts of your contract.
The bigger an entrypoint is, the more you save by putting it into lazy storage.
If you have a tiny entrypoint, it may be useless to put it into lazy storage.

One possible modification of this approach is to use strings as `big_map` keys.
In this case, you can put all the entrypoints of the same type into one `big_map`.
Thus the storage type will be smaller and unwrapping the storage to get the necessary `big_map` will be cheaper.
However, getting an entrypoint from the `big_map` will be more expensive because strings are larger than booleans.

Apart from decreasing gas costs, this approach can be used to decrease the contract size.
If you can't originate your smart contract due to oversized operation, you can apply this approach and supply empty `big_map`s during origination.
After that, fill the `big_map`s in multiple steps.
You need to add an additional entrypoint for that.

This approach can be applied with different granularity as well.
Splitting all the code into entrypoints is a good idea because you know that there is no way to execute more than one entrypoint at once.
However, there can be other cases when you know that.
For example, when you have `IF { large_code1 } { large_code2 }` you can have a `big_map` specifically for this piece of code and put both `large_code1` and `large_code2` into this `big_map`.
Both branches of `IF` will have a small size in this case, they will both do `GET`, but only one of these `GET`s will be interpreted and consume amount of gas proportional to the size of `large_code`.

A side advantage of this approach is that you can make your contract partially upgradeable.
You can have a special address that is allowed to update lambdas in the `big_map`s.
Thus, if you find a bug, you will be able to fix it.
If you don't want any upgradeability, you can reject all updates.

In the end, we must notice a drawback of this approach: it complicates the usage and the distribution of the contract.
Normally, you have a `.tz` file with all of the contract code and you can distribute just this one file.
Initial storage construction may be non-trivial, but usually the storage itself is relatively small (compared to the size of the code).
If you put some code into the storage, you have to distribute that code separately.
If that code doesn't fit into one origination operation, the deployment procedure will involve multiple steps.
For this reason, we recommend using simpler optimizations first and resorting to this one only if everything else is not enough.

Manually applying this approach to each smart contract is error-prone and tedious, so we believe it should be implemented in high-level languages, tools and libraries.
We have implemented it in a generic way in our Lorentz eDSL and we are incorporating it into Indigo eDSL.
We didn't apply it directly in our contracts, but:
1. Our [TZBTC](https://github.com/tz-wrapped/tezos-btc) smart contract is upgradeable and it gives us lazy processing of code for free.
That's a consequence of our upgradeability approach.
2. In one private smart contract, we apply an ad-hoc variation of this approach to put only two entrypoints into the storage.

### Be careful with strict types of variable size

`big_map` is the only lazy type in Tezos.
There are several strict types that can be arbitrarily large: `list`, `map`, `set`, `string`, `bytes`.
Gas costs associated with values of these types depend on their size.
If you use any of these types, it's very important to make sure they can't grow indefinitely, otherwise worst case gas consumption will be unbounded and your contract may surprisingly become unusable after being in use for a long time.

Ideally, you should statically ensure that all values of strict types have a limit on their size.
Example: you work with `set nat` and always check that `i < 10` before inserting `i` into the set.
If it's not the case, i. e. if an unbounded value can appear on the stack during your contract execution, you should specify additional constraints on the parameter and initial storage of your contract (as part of your contract's documentation). Examples:
1. Your contract takes a `list` parameter (maybe as part of a compound data type).
No matter what you do with this list (even if you ignore it), it can't be arbitrarily large because converting it into the typed representation consumes gas proportional to the length of the list.
If your contract is big, this limit can be quite small, it can be smaller than the users of your contract expect.
For this reason, it's important to compute this limit in advance and consider the scenario when someone wants to pass a larger list.
If there is some client software, it may call such an entrypoint multiple times with a list of fixed size.
In this case, you should compute the appropriate size of a "batch".
2. If your contract takes a `string` parameter the same reasoning applies, though it can be less obvious.
For example, imagine that your contract takes a `string` that contains some information about a person, hashes it and inserts into a `big_map`.
For a long time, users of this contract only pass a surname of the string and you expect it to be short, let's say less than 100 characters.
And then one day someone decides to submit longer data that has more than 1000 characters and gets an error due to gas consumption.
It might be unexpected for your users, so it's better to explicitly state it in advance.
3. Your contract has a `set` inside its storage, for example it stores data about elephants.
Additional constraint: one can't add more than `M` elephants to the contract.

These additional constraints may or may not be acceptable in your particular case, but you should always consider them in advance.
Usually, you want to ensure at least the following:
1. A non-authorized call (i. e. that doesn't check `SENDER`) cannot increase gas consumption of any future call.
2. An authorized call cannot increase gas consumption of future calls made by other entities.
For example, imagine that your storage contains `big_map address x` where `x` is an unbounded type (let's say `list`).
Let's call this `big_map` `xs`.
Alice can update `xs[alice_addr]` but can't update `xs` for any other address.
If any other address beside Alice's one can make a successful call that uses `xs[alice_addr]` value, that most likely indicates a problem because in this case Alice can prevent Bob from using a certain functionality of the contract.
If Alice is the only one who can access `xs[alice_addr]`, that might be not a big problem because Alice can only harm herself.

To sum up:
1. You should analyze maximal sizes of all the strict (i. e. not `big_map`) values that can appear on the stack during the interpretation of your contract.
2. If possible, it's better to avoid using unbounded values and use `big_map`s so that gas consumption stays constant regardless of the parameter and storage.
3. Don't forget to stress test your smart contract by calling its entrypoints with large arguments (if there are variable-length arguments) many times.
Making many transactions to a contract in a testnet can be time-consuming, so it's better to run a temporary network with short block periods (e. g. 1 second) and make transactions in batches.

### Cache computed values

The idea is to avoid applying a sequence of instructions to the same value more than once.
Let's say there is a sequence of instructions that appears more than once in your smart contract and operates on the first `n` elements of the stack.
If you somehow know that the first `n` elements of the stack are equal in two places where this sequence is used, you can cache the result of this computation by remembering and tracking its position on the stack.
You should make sure it's present on the stack until the last time it's needed.

Note that applying `DUP` and `DUG` to an item located somewhere deep on the stack is not entirely free, so this "caching" makes sense only for:
* Non-trivial sequences of instructions consisting of at least ≈5 instructions OR
* expensive instructions, e. g. `UNPACK`, `CONTRACT`, or `GET` from a `big_map`.
For expensive instructions, it can be beneficial to cache the result of a single instruction (which technically still can be called a sequence).

In large smart contracts, an entrypoint often affects only some part of the storage while the rest of it stays constant.
If a sequence of instructions depends only on the constants part of the storage, its result can be cached this way.

#### Storage decomposition

A specific example of this optimization is storage decomposition.
Typically a storage consists of many fields represented as a large nested `pair` type.
Accessing a single field involves multiple `CAR` and `CDR` instructions and it's a frequent operation.
Instead of doing `CAR; CDR; CDR; CAR` whenever we want to access a certain field, we can do it once and remember the position of this field.
Then, instead of bringing up the whole storage to the top of the stack, we can move only the field we need.

Storage decomposition can be applied to modifiable fields as well.
We can update the value stored on the stack and then, in the end, construct the updated final storage.

### Put lambdas on the stack with care

A Michelson smart contract contains a single list of instructions.
Unlike most of high level languages, it does not have named functions or procedures.
It has only anonymous functions — lambdas — that can be `PUSH`'ed onto the stack and `EXEC`'uted.

Programming large smart contracts as a single list of instructions without the ability to define a function and call it by name is too hard and inconvenient.
Nearly all languages built on top of Michelson offer some way to define functions and use them in Michelson code.
There are two possible ways to work with such functions:
1. Inline a function into the place where it is used.
2. Put it onto the stack as a `lambda` and then `EXEC` it.

Let's imagine a function that multiples the value on top of the stack by two.
It can be implemented as
> `f = DUP; ADD`

Somewhere in the contract we want to use this function:
> `PUSH nat 2; f`

In the first case, the generated Michelson code will be:
> `PUSH nat 2; DUP; ADD`

while in the second case, it will be:
> `PUSH nat 2; LAMBDA nat nat { DUP; ADD }; SWAP; EXEC`

The second case looks more verbose and less efficient.
That's true if `f` is small or used in a small number of places (e. g. just once).
However, if the body of `f` was large and `f` was used in multiple places, using lambdas would be more beneficial.
We can use `LAMBDA` once to put `f` onto the stack and remember its position.
Then each usage of `f` will look like `DUG n; SWAP; EXEC`.
The longer your `f` is, the more you can save this way.

So, as you can see, sometimes it's better to inline functions, sometimes it's better to put them as lambdas onto stack and `EXEC`ute them.
Ideally you should be able to use both approaches and pick the best one for each case.
Specifically, there are two ways to optimize your smart contract:
1. If there is a piece of code that appears in more than one place, it may be beneficial to extract it into a lambda.
2. If there is a lambda that is small or used in a small number of places, it may be beneficial to inline it because executing a lambda adds some overhead.

### Apply automatic optimizer

There is a number of optimizations that can be automatically applied using _rewrite rules_.
The idea is to replace a sequence of instructions `s₁` with a sequence `s₂` that has equivalent semantics and type but is more efficient.
For example:
1. `DIP { foo }; DROP` is equivalent to `DROP; foo`.
2. `SWAP; SWAP` is equivalent to the empty sequence.
3. `SWAP; ADD` is equivalent to `ADD`.
4. `DIP { x }; DIP { y }` is equivalent to `DIP { x; y }`.

There are many other patterns that can be automatically optimized.
Even if you manually write low-level Michelson code yourself, you may end up having a piece of code that can be trivially optimized.
If you use a high-level language, it will most likely produce non-optimal code.
For example, you may have a function that returns `a, b` on top of the stack and ends with `SWAP`, but its user may need `b, a`.
High-level code may look fine, there is no explicit `SWAP; SWAP`, but once you glue everything together into low-level Michelson you may notice some redundancy.

The best way to apply these rewrite rules is to implement a tool that would do it automatically.
If you don't want to implement it yourself, we've got you covered.
Our tool [`morley`](https://gitlab.com/morley-framework/morley/-/blob/e2d3cf9197804a2cfa92715e1a4d640fe5b87af7/code/morley) can automatically optimize Michelson smart contracts.
If you have `docker`, you just need to:
1. Get the script: `curl https://gitlab.com/morley-framework/morley/-/raw/e2d3cf9197804a2cfa92715e1a4d640fe5b87af7/scripts/morley.sh > morley.sh`.
2. Run `./morley.sh optimize --contract contract.tz --output optimal-contract.tz`.

Current version of our optimizer managed to make all our smart contracts smaller, but not significantly.
We occasionally add more rewrite rules to it, so the benefits of our optimizer can be increased in future versions.
Still, it's unlikely that it will ever optimize your contract by more than 10%.
However, since this optimization is done automatically we think it makes sense to apply it to all smart contracts.
Our high-level language Indigo (and Lorentz that is now part of Indigo) applies the optimizer by default (though it's possible to disable that).

### Pay attention to string and bytes constants

As was stated before, we are primarily interested in optimizing three things: total number of instructions, total size of variable-length constants and interpretation of expensive instructions.
This section is about the second item.

From our experience there are two main cases when you might need to put variable-length constants into smart contract code:
1. Error messages.
2. Statically known `big_map` keys.
For example, in the aforementioned [lazy code processing](#Lazy-processing-of-code) approach, you can put all entrypoints with the same argument into one `big_map` indexed by `string`, and then use entrypoint name as a key.
Or you can implement an untyped storage with type `big_map string bytes`.
We use this approach to implement upgradeable smart contracts.

There is a certain trade-off associated with using string constants in this case.
Longer strings are more descriptive but more expensive.
Basically, you can optimize your smart contract by making its code or errors less descriptive.
As a general rule it makes sense make keep all strings as short as possible while keeping them sufficiently descriptive.
Michelson lets you fail with any value, not necessarily a string.
But if you want the users of your contract to be able to get some information in error cases, most likely you should include strings into your errors.
Existing token standards for Tezos require errors to contain certain strings in common cases (e. g. not enough balance).
If your contract is too big, you can't throw away anything from it, and other optimizations do not help, you can resort to using more compact types in errors:
* Use numeric error codes instead of strings and keep a registry of error codes off-chain.
If there is a client program developed specifically for your smart contracts, it might be a decent optimization since the client can map error codes to descriptive messages.
We have made this optimization in one our smart contract that was very large and it was really hard to sufficiently optimize it.
* Fail with `UNIT` (maybe via the `FAIL` macro).
That's an ultimate optimization that is totally not recommended because it greately spoils user experience.
When the contract fails, users have no way to understand what went wrong (other than staring at source code and thinking or modifying it and simulating the transfer).

### Optimize high level languages

Large Michelson smart contracts are usually written using high-level languages.
In most cases, a high-level language adds some overhead to the resulting Michelson code.
An automatic optimizer can reduce this overhead but it can't eliminate it fully.
The choice of a high-level language can be more important than all optimizations.
Of course, the efficiency and clarity of generated Michelson code is not the only criterion affecting the choice of a language.
But it's an important criterion that you should take into account before writing a smart contract.

An important task for language developers is to ensure that Michelson code produced by their languages is efficient.
Ideally, smart contract languages should take care of all concerns related to gas consumption and contract size, including:
1. Whether to inline a function or put it onto the stack as a lambda.
2. Storage decomposition.
3. Common subexpression elimination.
It's another specific case of caching that it applicable to high-level languages.
If there is an expression like `a + 2 * b - c * 9` that is used in more than one place, it's more efficient to compute it once and store on the stack than to recompute it in all places where it is used.
4. Lazy code processing.
A regular contract that has all its code in the `code` section can be transformed to a contract with lazy processing (that stores all code in `big_map`s) automatically.
High-level languages can provide this ability out of the box (of course, if the user explicitly enables it).

## Suggestions for the core protocol

After presenting a number of tips on writing efficient Michelson smart contracts, we would like to share some suggestions on how the core protocol can be updated to facilitate smart contract development:
1. When an operation consists of multiple internal operations and a given smart contract is called more than once, its code should be turned into typed representation only once.
2. When a smart contract is called, only the relevant part of it (i.e. the called entrypoint) should be turned into the typed representation.
Code of other entrypoints should not affect the gas cost of the operation (or should affect it to a small extent).
If this change is made, the previous point will only apply to the case when a specific entrypoint of a given smart contract is called more than once.
3. We think that the operation size limit might be too strict.
In Carthage, the hard gas limit was increased by 30%, but the operation size limit was not changed.
Origination operations are the largest ones, and we think that one possibility could be to increase the operation size limit only when origination is involved.
That said, figuring out the optimal operation size limit is a separate topic worth separate research.

Note that in order to make the optimization (2) we need to find unreachable code based on parameter value, which can be difficult or infeasible in general case.
Entrypoints machinery is a lightweight extensions of Michelson which does not explicitly split smart contracts into disjunctive parts.
We see the following options:
1. Make this optimization work only in specific cases, for example when a smart contract starts with the `UNPAIR` macro followed by nested `IF_LEFT` applied to parameter.
In this case there are multiple disjunctive parts of the contract, and only one of them should be considered in runtime.
We believe it should cover majority of large smart contracts.
2. Make some changes to the Michelson language by letting smart contract developers explicitly split smart contracts into disjunctive parts.

We believe that further research of this idea should be conducted, it's left out of scope of this article.

## Conclusion

1. Tezos imposes hard limits on gas consumption and operation size.
In some cases they may seem overly restricted and force you to optimize your smart contracts.
2. The primary target for optimization is the total number of instructions in your contract.
Interpretation gas costs are usually much smaller than everything else, so major part of gas consumption can be computed statically.
It might be not the case if you, for example, pass very large objects as a parameter (and then loop over them) or `UNPACK` large values of large types.
But these cases are rare.
3. In the current version of Tezos, inter-contract communication is complicated and expensive.
We recommend writing monolithic contracts if possible.
4. It's important to stress test your smart contracts and know their limits.
If you don't pay attention to gas costs, your smart contract can be vulnerable to attacks that make it unusable by increasing the size of data it works with.
5. A cheap (but not very efficient) way to optimize your smart contract is to apply an automatic optimizer.
6. A very important factor that affects the efficiency of your Michelson smart contract is the choice of the high-level language.
Two smart contracts with the same semantics written in different languages can compile to substantially different Michelson code with substantially different gas consumption.
High-level languages should produce efficient code and handle all performance concerns automatically.

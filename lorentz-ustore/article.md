# Lorentz: type-safe upgradeable storages

Once deployed, large-scale contracts can sometimes become outdated and require upgrades.
Use cases for this include not only adding new features but also fixing bugs that, unfortunately, tend to occur even with well-established development processes.

Michelson does not yet provide build-in capabilities for changing an already deployed contract, thus upgradeability has to be supported explicitly from within the contract code.

In this article, we'll show you how to make a smart contract with upgradable storage in Lorentz.
In the next article of the series, we'll cover code upgrades as well.

But first, let's settle on how upgradeability in Tezos should look like.

## In-place upgradeability

By an upgradeable contract, we mean a contract with the following properties:

1. Its storage format can be changed: fields can be added or removed;
2. Its code can be modified.

The reader may find [our analysis of upgradeability tecnhiques](https://gitlab.com/morley-framework/morley-upgradeable/-/blob/f72475818bcb434ccb20bbd9fea4143823ce5add/docs/upgradeableContracts.md) interesting as the document broadly covers the theme of contracts upgrade in Tezos.
Further, we will just briefly mention the points related to our topic.

### How to create an upgradeable contract on Tezos

There are two main approaches to achieving upgradeability that are suitable in the case of Tezos:

1. On upgrade, originate the contract from scratch and migrate its storage. In case address immutability is desired (usually it is), use a separate _proxy_ contract that delegates all the calls to the relevant instance of the main contract.
2. Keep storage entries and code of entrypoints in a packed `bytes` form in a `big_map`, and update them when necessary.

While the proxy approach may seem simpler at first glance, it has a bunch of problems:

* Migrating large storage may be extremely expensive in terms of fees and idle time.
* Authorization techniques based on the `SENDER` instruction do not work when a proxy contract is involved.
* Updating the contract interface is still not possible without re-originating the proxy.

That's why the current best approach is using contracts upgradeable in-place, despite the increased fees.
However, this also imposes a severe problem for the contract developer.

When the entire storage and code are kept within a `big_map`, working with them becomes much less convenient: the built-in type system of Michelson does not protect us from some kinds of mistakes anymore.
We need proper support for our new type of contracts directly in the language engine.

In this article, we are going to focus on making contract storage upgradeable by using the `big_map` approach with Lorentz.

## Upgradeable storage

One of the superior features of Lorentz language is that such primitives can be quickly implemented once the need in them is acknowledged. This process can take place independently from the development of the language core.

To demonstrate the upgradeability functionality, we will start writing a sketch of a simple ledger contract.

### Storage representation

First, let's figure out how the entries in our upgradeable storage have to be represented by the framework.

Apparently, all entries have to appear in a single `big_map`, the type of which must be the same in all versions of the contract.
Since this map may need to contain entries of various types, we have to assume that keys and values of this map are `bytes` and probably contain something packed (where "packed" has the semantics corresponding to the [`PACK` instruction](https://tezos.gitlab.io/008/michelson.html#operations-on-bytes)).

Next observation: the storage of any contract has only two kinds of entries: strict fields and lazy maps.

* Strict fields are regular `PACK`-able types like `nat`, `string`, `list`, `set`, `map`.

  Under "strict", we here mean only that access to the entry requires deserializing the entire entry:
  deserialization happens when the field is requested due to the nature of the underlying `big_map`.
  In simple contract storage represented by a product/sum type the behaviour is sligtly different, there all non-`big_map` fields are deserialized before the contract code is even run.

* Lazy map is very much similar to `big_map`, it deserializes a value in the map on demand.

  The downside is that entries of a lazy map cannot be iterated (instructions like `iter` or `size` are not applicable here).

To represent a lazy map entry, we could pack its key and value and put them into the `big_map`.
However, this way different submaps could collide.
So we put a packed `(Pair submapName key)` as key instead.

In the case of fields, we simply use the packed field name as a key.

<details>
  <summary>Does it mean I should prefer shorter field names?</summary>

Since field names now appear as part of the contract code, a reasonable question may arise: should I declare shorter field names as an optimization measure?
Won't doing so harm the contract code readability?

Instead of changing the Lorentz code in such a way, we suggest tuning its compilation options.

Two of the predefined options allow modifying all the [strings](https://hackage.haskell.org/package/lorentz-0.10.0/docs/Lorentz-Run.html#v:coStringTransformerL) and [bytes](https://hackage.haskell.org/package/lorentz-0.10.0/docs/Lorentz-Run.html#v:coBytesTransformerL) within a Lorentz contract, respectively.
They can be applied to a Lorentz `Contract` as follows:

```haskell
import Control.Lens ((&~), (.=))

optimizedContract :: Contract Parameter Storage
optimizedContract = myContract &~ do
  cCompilationOptionsL . coStringTransformerL .=
    ( True  -- visit lambdas
    , stringsTransform
    )

stringsTransform :: MText -> MText
stringsTransform = \case
  [mt|ledger|] -> [mt|l|]
  [mt|totalSupply|] -> [mt|ts|]
  other -> other
```

---

</details>

### Lorentz interface

It is clear that working with the mentioned storage representation manually is too cumbersome and unsafe for a language that is supposed to be strictly typed. This has to be addressed by the framework.

As promised, further we will try to write a proof-of-concept contract — a simple upgradeable ledger.
This contract has to store balances for each address and the total amount of tokens held within the contract.

To define upgradeable storage with the mentioned structure, in Lorentz we write:

```haskell
import Lorentz
import Lorentz.UStore  -- from `morley-upgradeable` package

data StoreTemplate = StoreTemplate
  { ledger :: Address |~> Natural       -- lazy submap
  , totalSupply :: UStoreField Natural  -- field
  } deriving stock (Generic)

type Storage = UStore StoreTemplate
```

Here the `UStore` type stands for upgradeable storage.
It has one type argument, which we call _template_, and that defines the desired structure of the storage.

Methods for working with this storage are pretty similar to the existing ones.
In case we need to work with a submap, we can use `ustoreMem`, `ustoreGet` and `ustoreUpdate` instructions that mimic the respective Michelson instructions for working with plain maps.
Also, we provide additional `ustoreDelete`, `ustoreInsert` and `ustoreInsertNew` macros to cover the most common use cases.

```haskell
creditTo :: Address : Natural : Storage : s :-> Storage : s
creditTo = do
  dupN @3; dupN @2
  ustoreGet #ledger; fromOption 0

  stackType @(Natural : Address : Natural : Storage : _)
  swap; dip add
  ustoreInsert #ledger
```

To access fields, we can use `ustoreGetField` and `ustoreSetField` that are similar to `getField` and `setField` provided by Lorentz for plain datatypes:

```haskell
creditTo :: forall s. Address : Natural : Storage : s :-> Storage : s
creditTo = do
  dip $ do
    dup @Natural; dip $ do
      dip $ ustoreGetField #totalSupply
      add
      ustoreSetField #totalSupply

  stackType @(Address : Natural : Storage : s)
  ... -- code that updates the 'ledger' map
```

All the methods for working with `UStore` can be found in [the documentation on Hackage](https://hackage.haskell.org/package/morley-upgradeable-0.3/docs/Lorentz-UStore.html).

The storage template makes sure that all fields and lazy maps are used correctly.

For instance, the type system now ensures that each field is used with the same type across the entire contract code.

```haskell
f :: Storage : s :-> Storage : s
f = do push @Integer (-1); ustoreSetField #totalSupply

---

(src:5:28) error:
    • Couldn't match type ‘Natural’ with ‘Integer’
```

The compiler can even give hints on the type required at some point:

```haskell
f :: Storage : s :-> Storage : s
f = do push _; ustoreSetField #totalSupply

---

(src:5:13) error:
    • Found hole: _ :: Natural
    • In the first argument of ‘push’, namely ‘_’
```

A typo in a field name now will be handled at compilation time.

And as the reader can note, the interface does not let the user accidentally remove a field from the `big_map`.
Fields are guaranteed to be there, and there is no need to manually write something like `GET; ASSERT_SOME` every time to access a field.

Here one may ask: what about storage initialization?
Can I forget to initialize some fields when deploying my contract?

In Haskell world, we handle this gracefully: `UStore StoreTemplate` can be constructed from `StoreTemplate` value, and initializing `StoreTemplate` is a completely type-safe action:

```haskell
initStorage :: Storage
initStorage = mkUStore StoreTemplate
  { ledger = UStoreSubMap mempty
  , totalSupply = UStoreField 0
  }
```

This value can later be passed in our test framework, or printed in various formats for further contract origination with a different tool:

```haskell
initStorageAsMichelson = printLorentzValue True initStorage

-- >>> putStrLn initStorageAsMichelson
-- { Elt 0x05010000000b746f74616c537570706c79 0x050000 }
```

```haskell
-- ↓ from 'aeson-pretty' package
import qualified Data.Aeson.Encode.Pretty as Json
import Morley.Micheline (toExpression)

initStorageAsMicheline = Json.encode $ toExpression $ toVal initStorage

{- >>> putTextLn (decodeUtf8 initStorageAsMicheline)
[
    {
        "args": [
            {
                "bytes": "05010000000b746f74616c537570706c79"
            },
            {
                "bytes": "050000"
            }
        ],
        "prim": "Elt"
    }
]
-}
```

As one can fairly note, this binary representation of keys and values in the map is not very convenient to work with manually.
However, most of the time, it is hidden from the end-user, and even blockchain explorers nowadays can [detect and interpret](https://edo2net.tzkt.io/KT1SdnYQA5Sgfy71nixM3wc79EMWmn5vTFmm/storage) packed data.

To construct storage which depends on user's input, we usually write a small dedicated command-line utility.
Other frameworks (e.g. used by middleware) can also try to construct such storage, but doing this conveniently is a matter of writing a standard and libraries implementing it.

### Polymorphism

Now, what if I'm writing a library that defines useful common primitives for smart contracts, does it mean that methods working with storage have to be implemented twice — for plain types and `UStore`?

Not necessarily.

We provide methods for working with storages in a polymorphic manner: `stGetField`, `stSetField`, `stInsert`, and others.
The full list of methods can be found in [the Lorentz docs on Hackage](https://hackage.haskell.org/package/lorentz-0.11.0/docs/Lorentz-StoreClass.html#g:5).

So, for instance, the code of `creditTo` defined above can be rewritten as:

```haskell
-- | Constraint on storage used in our ledger.
type StorageC store =
  ( StoreHasSubmap store "ledger" Address Natural
  , StoreHasField store "totalSupply" Natural
  )
-- (A)

creditTo
  :: StorageC store  -- (A)
  => Address : Natural : store : s :-> store : s
creditTo = do
  -- Update total supply
  dip $ do
    dup @Natural; dip $ do
      dip $ stGetField #totalSupply  -- (B)
      add
      stSetField #totalSupply  -- (B)

  -- Update ledger
  dupN @3; dupN @2
  stGet #ledger; fromOption 0  -- (B)
  swap; dip add
  stInsert #ledger  -- (B)
```

Replaced calls of `ustore*` methods are marked with `(B)`.

This implementation will work when as `store` we pass our `Storage = UStore template`.
The exact layout used by `template` type is no longer relevant, as long as the same `ledger` submap and `totalSupply` field are present.
This is achieved via adding a `StorageC` constraint (see `(A)`).

A plain product type can also be used as the storage passed to our method.
We only have to provide a `StoreHasField` instance that specifies how the required fields can be accessed.

```haskell
data Storage = Storage
  { ledger :: BigMap Address Natural
  , totalSupply :: Natural
  } deriving stock (Generic)
    deriving anyclass (IsoValue)

instance HasFieldOfType Storage name field =>
         StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

-- ↑ Note that in older versions of `lorentz` package some different
-- instances may be necessary; just follow the error messages.
```

<details>
  <summary>Why some new instances?</summary>

The reader may ask, why this new `StoreHasField` has to be managed by the user instead of being derived implicitly.

This mechanism of `StoreHasField` and `StoreHasSubmap` instances is a very powerful tool as it allows locating fields and submaps in complex cases, even when the necessary entry is not physically present in the storage in the necessary form.

A real-life story: one of our FA1.2 contracts, implemented back in times when Michelson allowed only one `big_map` across the storage, had to keep balances and approvals within one map as `BigMap Address (Natural, Map Address, Natural)`.
When `big_map` restriction was lifted, we simplified the methods in our FA1.2 base library, so now they expected two separate `BigMap Address Natural` and `BigMap (Address, Address) Natural`.

Nevertheless, in the repository with our contract, we managed to switch to the new version of the library without changing the storage format at all (and thus avoid the need to migrate the already deployed contracts) just by adding the appropriate `StoreHasSubmap` instances.

This demonstrates how convenient it can be to split the contract logic into several layers of abstraction, in our case — a dedicated layer for the business logic and a completely separate layer for the tricky map element access.
Such opportunities are hardly achievable in smart contract languges that lack polymorphism, where developers have to resort to code duplication each time their types change.

---

</details>


One of our library contracts using the polymorphic storage access is [FA1.2 ManagedLedger](https://gitlab.com/morley-framework/morley-ledgers/-/blob/c92008db75363a50adf469c44a39e17843c311ec/code/morley-ledgers/src/Lorentz/Contracts/ManagedLedger/Impl.hs).
We used it to implement various end-product contracts that had to include FA1.2 functionality¹.

---

¹ Remember that splitting code into multiple contracts is quite expensive in Michelson. This is explained in the "Beware inter-contract communication" section of [this post](https://medium.com/tqtezos/how-to-minimize-transaction-costs-of-tezos-smart-contracts-9962347faf64).

### Composability

There is a reasonable tendency to split contracts into small reusable components.
For instance, our production ledger contract might consist of a FA2 core + administration + pausing components.
In this regard, making each component describe its own part of the storage is desirable.

Our upgradeable storage does not restrict the developer in applying the mentioned practices since the storage template allows nested entries:

```haskell
---- All.hs module
---------------------------------

-- All the components consolidated

data StoreTemplate = StoreTemplate
  { ledgerStore :: LedgerStoreTemplate
  , adminStore  :: AdminStoreTemplate
  , pausedStore :: PauseStoreTemplate
  } deriving stock (Generic)

initStore :: Address -> StoreTemplate
initStore adminAddr = StoreTemplate
  { ledgerStore = initLedgerStore
  , adminStore  = initAdminStore adminAddr
  , pausedStore = initPauseStore
  }

type Storage = UStore StoreTemplate

---- Ledger.hs module
---------------------------------

data LedgerStoreTemplate = LedgerStoreTemplate
  { ledger :: Address |~> Natural
  , totalSupply :: UStoreField Natural
  } deriving stock (Generic)

initLedgerStore :: LedgerStoreTemplate
initLedgerStore = LedgerStoreTemplate
  { ledger = UStoreSubMap mempty
  , totalSupply = UStoreField 0
  }

---- Admininstation.hs module
---------------------------------

data AdminStoreTemplate = AdminStoreTemplate
  { admin :: UStoreField Address
  , pendingNextAdmin :: UStoreField Address
    -- ↑ for two-phase ownership transfer
  } deriving stock (Generic)

initAdminStore :: Address -> AdminStoreTemplate
initAdminStore adminAddr = AdminStoreTemplate
  { admin = adminAddr
  , pendingNextAdmin = adminAddr
  }

---- Pausable.hs module
---------------------------------

data PauseStoreTemplate = PauseStoreTemplate
  { paused :: Bool
  } deriving stock (Generic)

initPauseStore :: PauseStoreTemplate
initPauseStore = PauseStoreTemplate False

```

`All.hs` module gives an overall look on the contract storage.
Next `Ledger.hs`, `Administration.hs` and `Pausable.hs` modules define storages of each of the subcomponent (in a real life these modules would be put to separate files).

In `All.hs` part we glue all the subcomponent's storages together and provide the initial storage value.
Note that this module does not need to know anything about the inner representation of subcomponents' storages.

From the perspective of the contract code, this storage still has a flat structure.
This means that our polymorphic `creditTo` method will work on the new complex storage without any changes.

## Conclusions

In this article, we have considered Lorentz's approach to storage for in-place upgradeable contracts.
While implementing functionality like this in a type-safe manner would not be possible in Michelson without including a dedicated feature into the language core, addressing this at Lorentz level does not make a problem.

In the next posts, we are going to touch upgradeable entrypoints and the most interesting part of the story: type-safe contract migrations.

While this article appears quite late in the series, we encountered the need to write a production-scale upgradeable contract almost immediately after the Lorentz language was born.
So `UStore` feature is almost as old as support for product and sum types and should be quite stable by now.

At the moment, we are developing a generic interface for upgradeable contracts, it will be included in the Tezos development proposals repository as [tzip-18](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-18/tzip-18.md).

The implementation of `UStore`, as well as some examples of its use, can be found in the [morley-upgradeable](https://gitlab.com/morley-framework/morley-upgradeable/) repository.

Stay with us!

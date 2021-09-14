# Lorentz: type-safe storage migrations

In [the previous post](https://serokell.io/blog/lorentz-upgradeable-storage), we considered an implementation of upgradeable storage.
In Lorentz, it is represented as the `UStore` type, which is a type-safe wrapper over `big_map`.
It allows each contract version to have its own set of fields and lazy maps.

The following question may arise: how do we make this storage migratable?
One generic and convenient way would be to include an entrypoint with `lambda storage storage` argument into our contract.
It should be callable only by the administrator and update the storage with the provided lambda.
We will concentrate on this approach.
More on this you can find in our [analysis of upgradeability techniques](https://gitlab.com/morley-framework/morley-upgradeable/-/blob/f72475818bcb434ccb20bbd9fea4143823ce5add/docs/upgradeableContracts.md#section-2-administrator-forced-upgrades).

Next question: if in Lorentz fields and maps available in storage are handled in types, can we make storage migration type-safe too?

In this article, we are going to see how Lorentz addresses this wish.

Disclaimer: facilities presented here are still at a prototype stage.
For good or not, we haven't yet had the need to migrate any production contracts written by us. The migration framework will be completed once the demand for it arises.

## Simple migration

It would be nice when the framework ensures the storage to be entirely updated.
Lorentz provides special primitives for safely writing the storage migration script.

As an example, let's consider the migration of the following storage:

```haskell
data MyTemplateV1 = MyTemplate
  { name :: UStoreField MText
  , sum :: UStoreField Natural
  , useless :: UStoreField ()
  , bytes :: Integer |~> ByteString
  } deriving stock Generic

type StorageV1 = UStore MyTemplateV1
```

into this one:

```haskell
data MyTemplateV2 = MyTemplate
  { theName :: UStoreField MText
  , sum :: UStoreField IntegerWe
  , count :: UStoreField Natural
  , bytes :: Integer |~> ByteString
  } deriving stock (Eq, Show, Generic)

type StorageV2 = UStore MyTemplateV2
```

To obtain the model of version 2, we performed the following transformations to the version 1 model:
1. `name` field was renamed to `theNamed`;
2. `sum` field changed its type;
3. `count` field was added.
4. `useless` field was removed.

Now let's see how to write down these changes for our contract.

First, we insert a template of migration code:

```haskell
migration :: UStoreMigration MyTemplateV1 MyTemplateV2
migration = mkUStoreMigration do
  -- leaving empty for now
  migrationFinish
```

This is yet a stub for the migration.
The content of `mkUStoreMigration` is mere Lorentz code of type `[MUStore ...] :-> [MUStore ...]`.
It accepts the initial storage as its only stack argument and should return the fully migrated storage.
The storage is passed around as a `MUStore` type (meaning _mutable `UStore`_) that tracks the migration progress in types.

Notice the placed `migrationFinish` call at the end.
`migrationFinish` is a `nop`-like instruction that checks whether the migration is fully performed and raises a compilation error with the remaining migration steps if some are yet undone.
(Call of this function can be omitted, but then we would get a not-so-human-readable error).

In our case, we'll see:

```F#
• Migration is incomplete, remaining diff:
  + `theName`: field of type `MText`
  + `sum`: field of type `Integer`
  + `count`: field of type `Natural`
  - `name`: field of type `MText`
  - `sum`: field of type `Natural`
  - `useless`: field of type `()`
```

So, as expected, `bytes` should not be modified.
Now let's handle the remaining fields.

Similar to how `big_map` has `get`, `update`, and other instructions, for `MUStore`, we have dedicated specific methods suitable for migrations and that accounts for steps that have been performed.

First of all, let's add the `count` field, initializing it with zero:

```haskell
migration = mkUStoreMigration do
  push 0
  migrateAddField #count

  migrationFinish
```

Now, the point about `count` is removed from the error message, meaning that we covered this part of the diff.

Similarly, we want to remove the `useless` field:

```haskell
  ...

  migrateRemoveField #useless

  ...
```

In other cases, we don't want just to delete a field, rather pick it for further use.
In our migration, the `name` field should be fetched and put back as `theName`.

For this, there is a dedicated `migrateExtractField` that obtains a field value and marks it as removed from the old storage.

Let's use it here:

```haskell
  ...

  migrateExtractField #name
  push [mt|The |]; concat  -- Prefix the name with "The "
  migrateAddField #theName

  ...
```

The error message now says:

```F#
• Migration is incomplete, remaining diff:
  + `sum`: field of type Integer
  - `sum`: field of type Natural
```

Since `sum` has a different type in the new version, it has to be migrated as well.
We can change it in the very same way as with the `name` field.

At this point, our code compiles, meaning that the migration script is complete.

What if we tried to add an unnecessary field or delete a non-existing one?
Since all steps are tracked, we would get a compile-time error.

```haskell
migration = do
  ...

  push @Natural 0
  migrateAddField #number  -- ← "number" instead of "count"

  ...

```

--------------------------------------------------------

```F#
• Failed to find plain field or submap in store template
  Datatype `V2.MyTemplate` has no field "number"
• In a stmt of a 'do' block: migrateAddField #number
```

or:

```haskell
migration = do
  ...

  migrateExtractField #myName  -- ← "myName" instead of "name"
  L.push [mt|The |]; L.concat
  migrateAddField #theName

  ...
```

--------------------------------------------------------

```
• Failed to find plain field or submap in store template
  Datatype `V1.MyTemplate` has no field "myName"
• In a stmt of a 'do' block: migrateExtractField #myName
```

Finally, what do we do with the constructed `UStoreMigration` value?
In simple cases, we call `migrationToScript` to compile the migration to `MigrationScript`, which is just a wrapper over `Lambda (UStore ...) (UStore ...)`.
This migration script can later be printed, but most often it is just passed to the upgrading interface of the contract.

## Instant filling

One corner case of migration is a migration that solely adds fields.
Invoking `migrateAddField` manually for each field would be too tiresome, so we provide a couple of helpers to cover this case.

For instance, how does a migration from `V0` (empty storage) to `V1` look?
Usually, it is just an addition of all fields from `V1` storage.
It would be nice to construct a migration script as easily as we usually provide initial storage to a non-upgradeable contract.

And we can:

```haskell
migrationToV1 :: UStoreMigration () MyTemplateV1
migrationToV1 = fullUStore MyTemplate
  { name = UStoreField "name"
  , sum = UStoreField 0
  , useless = UStoreField ()
  , bytes = mempty
  }
```

We can also use this trick to fill only a part of `UStore` if it has a nested structure.

Say, in `V2` we also want to add storage for admin functionality like this:

```haskell
data AdminTemplate = AdminTemplate
  { admin :: Address
  , pendingAdmin :: Address
  } deriving (Generic)

initAdminStore :: Address -> AdminTemplate
initAdminStore admin = AdminTemplate admin admin
```

Once we include a new `admin` field into `MyTemplateV2`, the compiler will immediately tell us that our `migration` is again incomplete.
To fix that, instead of using `migrateAddField` to fill `admin` and `pendingAdmin` fields in the script as before, we can rather write:

```haskell
  ...

  migrateFillUStore (initAdminStore myAdmin)

  ...
```

So oftentimes there is even no need to write the migration script manually: one can just construct ready values in Haskell and pass them to the migration.
And fallback to the script when more control is necessary.

One common use case for this is entrypoints: the implementation of those is kept in the storage.
When the storage type changes, it is safer to update all the entrypoints since some of the code in them may not work with the new storage format.
For that, we can keep all the entrypoints as a separate part of `UStore`, and update them with `migrateFillUStore`.

Given the current contract code, it would be better to check entrypoint update requirements at runtime.
This will be supported in the future versions of Lorentz.

## Batched migration

One sad truth of life is that not every migration, if put into one transaction, would fit into the Tezos limits.
We need a way to split a migration into numerous scripts that would be applied sequentially.

Apparently, there is no way for the framework to see how to cut the script into pieces.
The user should be allowed to put explicit boundaries right into the script.

To handle this need, Lorentz provides a dedicated batched interface (note that the notion of "batching" here is parallel to Tezos' transaction batches).

If we rewrite our script above in terms of batch blocks, we'll get:

```haskell
batchedMigration :: UStoreMigration V1.MyTemplate V2.MyTemplate
batchedMigration = mkUStoreBatchedMigration $
  muBlock do
    L.push 0
    migrateAddField #count
  <-->
  muBlock do
    migrateRemoveField #useless
  <-->
  muBlock do
    migrateExtractField #name
    L.push [mt|The |]; L.concat
    migrateAddField #theName
  <-->
  muBlockNamed "change type of 'sum'" do
    migrateExtractField #sum
    L.int
    migrateAddField #sum
  <-->
  migrationFinish
```

At this point, we created the very same migration as before. The only difference is that it remembers the upgrading code as a set of isolated blocks.
The main invariant to keep in mind here — arbitrary rearrangement of the blocks shouldn't break the migration correctness.
So that even if migration blocks appear in the chain reordered, the migration still remains valid.

<details>
  <summary>Can't we just put all the migration parts in one Tezos transaction batch and so fix the order of execution?</summary>

  Transaction batches also have their limits.
  At the moment of writing, a transaction batch cannot require more than `10 * L` gas, where `L` is the hard gas limit for a single transaction.
  So the entire migration may not fit into a single batch.

</details>

Blocks can be named; this would affect the pretty-printed migration plan if the user wishes to check it out.
By default, the name just reflects the set of actions performed within the block, like `add "count"` or `remove "useless"`.
For the last block, we would get `remove "sum", add "sum` by default which may look unclear, so we assigned an explicit name to it.

Now we use `compileMigration` to slice the migration into parts.
As the first argument, it accepts a value of `MigrationBatching` type that describes the way to reorder and unite blocks into migration batches (one migration batch per transaction) to minimize the eventual number of transactions.

Multiple batching strategies are available.
Trivial ones are: `mbNoBatching`, which just unites all the blocks, and `mbBatchesAsIs`, which puts one block per transaction.

Oftentimes, `mbSeparateLambdas` is a sensible choice. It takes all the data in one transaction and every added field that looks like a stored entrypoint and puts them in a separate transaction.
Implementation of smarter strategies, in particular — taking actual gas and operation size limits into account — is left for future work.

An example: if we want to see how one of the trivial batching strategies work, we can write:

```haskell
λ> import Fmt (fmt, indentF)

λ> compiledMigration = compileMigration mbBatchesAsIs migrationBatched
λ> fmt $ "For my contract:\n" <> indentF 2 (renderMigrationPlan compiledMigration)

For my contract:
  Migration stages:
  1) add "count"
  2) remove "useless"
  3) remove "name", add "theName"
  4) change type of 'sum'
```

The compiled migration can later be converted into one or multiple `MigrationScript`s, depending on which batching strategy was used.

## Conclusion

In this article, we have considered how Lorentz allows for type-safe storage migrations.

It so happened that none of our production contracts required a migration yet, so this part of the framework is to be perfected.
For instance, migration of big maps is not yet supported at all, and some other wishes wait to be implemented.

Still, it is interesting to see how the framework can drive the user through the migration process and ensure that migration is complete.

In the next (third) post about upgradeability, we are going to consider the implementation of an upgradeable contract.

After that, we plan to cover topics like:

* How to start hacking with Lorentz in 15 minutes and which development techniques are available there.
* Some recent additions — high-level functionality like expressions and nested fields access befriended with manual stack management.
* Metaprogramming capabilities **_with_** polymorphism, or how to write extensible contracts where all versions are type-checked at once.

Stay with us!

# Lorentz: generating documentation for smart contracts

When developing tools and applications for public use, it is essential to provide complete and unambiguous documentation.
The same applies to smart contracts.

Even for medium-sized contracts maintaining documentation may become a burden.
Documentation tends to become outdated, its verification requires constant manual work, and the result still tends to diverge with the implementation in small but important parts.

In this article, we will consider how you can address this issue and maintain documentation with ease using Lorentz, an eDSL for Michelson smart contracts.

This post is aimed more at Tezos audience, so it won't touch the framework's implementation or any Haskell-specific details.

## Generation from the contract

Shortly speaking, our solution is embedding documentation pieces directly in contract code.
Final documentation can later be automatically derived from the contract code.
The resulting content is Markdown-formatted and can be rendered by GitHub/GitLab facilities or with the use of dedicated editors.

Being an eDSL, Lorentz can provide support for non-Michelson elements quite effortlessly (in fact, it already does so since it includes [the Morley extension](https://gitlab.com/morley-framework/morley/-/blob/13b57e14390c9bf77182b23fd97d8bdbc15f6a8a/code/morley/docs/language/morleyInstructions.md)).

Some parts of documentation can be deduced via analyzing the contract contents, while others are added on the developer's behalf.

Below, we look at how Lorentz contracts are documented in practice.

## Documenting my contract

Let's consider a simple counter contract:

```haskell
data Parameter
  = Add Natural
  | Sub Natural
  | Get (Void_ () Natural)

type Storage = Natural

contract :: Contract Parameter Integer
contract = defaultContract $ do
  unpair
  entryCaseSimple
    ( #cAdd /-> do
        add
        nil; pair
    , #cSub /-> do
        rsub
        isNat
        ifSome nop (failUsing [mt|Subtracted too much|])
        nil; pair
    , #cGet /-> void_ $ do
        drop @()
    )
```

_You can also see all code in the [autodoc example repository](https://gitlab.com/morley-framework/example-lorentz-autodoc/-/commits/autodoc-article)._

It is immediately possible to generate Markdown documentation for this contract:

```haskell
dumpContractDoc :: IO ()
dumpContractDoc =
  writeFile "Counter.md" $
    buildMarkdownDoc (finalizedAsIs contract)
```

You can see the produced documentation [here](https://gitlab.com/morley-framework/example-lorentz-autodoc/-/blob/autodoc-1/Counter.md).

This only describes the overall structure of the contract.
Entrypoints are deduced from the `entryCaseSimple` call, and possible failure cases are reflected in the `Errors` section.

However, our documentation does not yet explain what exactly the contract does, so we would like to add some comments.

Since in Lorentz documentation is an inherent part of the contract, descriptions can be inserted next to the code they are related to:

```haskell
contract = defaultContract $ do
  doc $ DDescription [md|
    A trivial counter contract.

    Used for demonstration purposes.
    |]

  unpair
  entryCaseSimple
    ( #cAdd /-> do
        doc $ DDescription "Increase the counter by given value."
        ...
    , #cSub /-> do
        doc $ DDescription "Decrease the counter by given value."
        ...
    , #cGet /-> do
        doc $ DDescription "Get the current counter value."
        ...
    )
```

Here `doc` stands for a special meta instruction that contains a so-called _doc item_, and `DDescription` is one of the base items used to document different parts of a contract.
Such comments are useful both to clarify code and as descriptions for the resulting contract documentation.

For a perfect result, we can additionally wrap the contract code into `docGroup "Counter"` at top-level. This will put the entire documentation under a `Counter` title.

The generated documentation now looks as follows:

![Documentation with descriptions added](https://user-images.githubusercontent.com/5394217/100778525-2afcc480-3418-11eb-998e-a50cf4387d09.png)

TODO: make sure that in the blogpost pictures are easily distinguished from the other text

Note that there is no need to include technical details in the description (like the type of each entrypoint's argument) because they are already inserted automatically.

### Adding custom types

Now a new requirement comes in: our contract should operate with tokens, not plain numbers.
How do we reflect this in the documentation?

Since a token is a semantically different notion than a number, we would like to create a special type for it:

```haskell
newtype Token = Token Natural

-- Parameter and Storage are updated respectively --
```

At this point, we can clearly distinguish in code whether a method accepts a token or a mere number.
The change will be reflected in the documentation.
Now it will mention the `Token` type everywhere instead of a plain `Natural`.
The only thing we need for our code to compile is to provide documentation for our new type:

```haskell
instance TypeHasDoc Token where
  typeDocMdDescription = "Tokens amount"
```

Now we get the following documentation for our type:

![Tokens type](https://user-images.githubusercontent.com/5394217/100784102-ba59a600-341f-11eb-8355-a8ecaaeea2eb.png)

### Adding custom errors

It is clear that for averagely-sized contracts, we already cannot afford to fail with just `unit` or a plain textual description – this would give the user insufficient clue on the reason for the failure.
A generic representation of an error assumes a `(tag, param)` format, where `tag` is a unique identifier of an error and `param` denotes error details in a machine-parsable format.

Let's apply this to our small contract.
On invalid subtraction, we would like to return a `(tag, balance - subtracted)` pair.

Lorentz already provides support for such an error format, and our error can be declared in it as follows:

```haskell
type instance ErrorArg "badSubtract" = Integer

instance CustomErrorHasDoc "badSubtract" where
  customErrClass = ErrClassActionException  -- normal user error
  customErrDocMdCause = "Too few tokens for subtraction"
  customErrArgumentSemantics = Just "diff between balance and subtracted amount"
```

Now the error is documented and usable in code.
Our definition additionally ensures that the `badSubtract` tag is always associated with the `Integer` argument across the contract, which should be convenient for middleware code that could potentially use our contract.

Now we can modify the contract implementation to use the new error:

```haskell
  #cSub /-> do
    rsub
    dup
    isnat
    if IsSome
      then do dip drop; coerceWrap
      else failCustom #badSubtract
    nil; pair
```

And the generated documentation is automatically updated both globally:

![BadSubtract error](https://user-images.githubusercontent.com/5394217/100785645-ff7ed780-3421-11eb-89cb-5348ed01217f.png)

and on per-entrypoint basis:

![BadSubtract error in entrypoint](https://user-images.githubusercontent.com/5394217/100785642-fdb51400-3421-11eb-8f47-ffc52be1afc1.png)

### Adding custom doc items

One of the core features of autodoc is that users can define their own doc items to describe various properties of the code.
These might include:
* Authorization checks;
* Special predicates on contract state, checked prior to executing the main logic;
* Created operations;
* Precautions for users;
* Some statistics on the code like pre-evaluated gas consumption.

Let's consider an example with authorization as the simplest one: our contract's storage should be updated only by calls from a specific address and fail otherwise.

To introduce the necessary contract logic, we will probably write something like:

```haskell
-- Definition of authorization error --

authorizeUpdate :: s :-> s
authorizeUpdate = do
  sender
  push adminAddress
  ifEq nop (failCustom_ #unauthorized)

contract =
  ...
  entryCaseSimple
    ( #cAdd /-> do
        authorizeUpdate
        ...
    , #cSub /-> do
        authorizeUpdate
        ...
    , #cGet /-> do
        ...
    )
```

Now we want to state in the documentation that our `add` and `sub` entrypoints require special access.
For this, we can declare a `DAuthorization` doc item:

```haskell
data DAuthorization = DAuthorization Address

instance DocItem DAuthorization where
  docItemPos = 10120  -- global position, doc items of the same type
                      -- are grouped and then sorted according to this
  docItemSectionName = Nothing

  docItemToMarkdown _ (DAuthorization addr) =
    mdSubsection "Authorization" $
      "The sender must be " <> mdTicked (pretty adminAddress) <> "."
```

And include it into our check:

```haskell
authorizeUpdate = do
  doc $ DAuthorization adminAddress
  ...
```

Note that if we ever need to update the authorization logic, just as we only have to update the implementation in a single place, we only need to update the documentation once, but it will be included in every related entrypoint.

Indeed, the case with a single allowed address is not very interesting and we could use mere `DDescription` here.

The more interesting (and quite real) case involves having several roles, and each entrypoint requiring the sender to belong to a specific subset of roles.
In situations like this, always keeping the documentation on par with the implementation becomes essential, but it's pretty cumbersome with the manual approach.
And in Lorentz, the implementation for such cases won't significantly differ from the example considered above.

One another use case for custom doc items is metadata collection.

As the reader may know, interfaces for Tezos smart contracts are now actively developed, and the recently approved [TZIP-16](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md) interface lets contracts expose various information like description, version, license, and other.
Normally the developer has to fill this metadata manually, but all the same information preferably should also be included in the documentation; some of this data is usually already defined in the contract's source code in some way.
To avoid the necessity in duplication, we [are going](https://gitlab.com/morley-framework/morley-metadata/-/issues/4) to make these parts of metadata derivable from the contract documentation as part of our TZIP-16 support.

By the way, documentation for types, errors, and entrypoints is also implemented as mere doc items, so the developer is free to describe their contents in any format allowed by Markdown.
`docGroup` calls can also accept arbitrary doc items that group contents together, passing a string just uses the predefined `DName` object.

## Tests on documentation

When it comes to the manual documentation writing, it is the developer's responsibility to ensure that docs remain consistent with code updates. Normally, a fair amount of time is spent on it.

With automatic generation in place, many issues are not possible anymore.
However, minimal consistency checks like "all the new endpoints are documented" are still necessary.

Most of this verification is mostly routine and can be performed, again, automatically via adding tests.

Writing the following test suite:

```haskell
test_Documentation :: [TestTree]
test_Documentation =
  runDocTests testLorentzDoc contract
```

will add a base set of common Lorentz doc tests for our contract, and that includes checks like
* All entrypoints are described;
* Storage is documented;
* All descriptions end with a dot (for the sake of uniform formatting).

Generally, we intend these test suites not only as a validation tool but also as a way to guide the developer through the process of writing neat and complete documentation.
So, for instance, we haven't documented the contract storage yet (making this optional was a design choice), and now tests will recommend us to use the `dStorage` doc item to include the contents of our storage into the doc.

Complying with these checks is generally a good practice.
On the other hand, they can be considered too restrictive in some situations.
For example, I may want to hide the contract's storage because its format may change and I don't want other developers to rely on its current structure.
For such cases, we provide an exclusions mechanism, and one can write:

```haskell
test_Documentation :: [TestTree]
test_Documentation =
  let tests = testLorentzDoc `excludeDocTests`
        [ testStorageIsDocumented
        ]
  in runDocTests tests contract
```

If changes in Lorentz take place and additional useful checks appear there, they will be automatically included in our tests once we depend on a newer version of Morley.
In case there is such a demand, the Morley framework can also be updated with a way to filter checks by category.
For example, it could let the user exclude all tests about formatting or, alternatively, leave only checks about the correctness of the resulting documentation.

Indeed, defining custom checks is also possible.

Cautious devs who want to make sure that ordering matches their expectations can optionally add tests like:

```haskell
test_Documentation_ordering :: [TestTree]
test_Documentation_ordering =
  [ -- authorization requirement is mentioned after
    -- the entrypoint description
    Proxy @DDescription `goesBefore` Proxy @DAuthorization
  ]
```

## Conclusion

In this article, we discussed the automatic documentation machinery provided by Lorentz.

It follows the "Everything as Code" principle and thus provides some benefits that are usually applicable to code, and even more:
* No need for text duplication;
* Leaving something undocumented is made difficult:
  + Test coverage ensures that this and other useful properties hold;
* Leaving documentation outdated is made difficult:
  + Documented properties are defined next to the respective logic implementation;
* Uniform formatting and text style;
* Easy navigation for a reader:
  + Cross-references connect the related pieces of the document;
  + Each section mentions exactly the points relevant to its topic.

The first version of autodoc has appeared more than a year ago and was immediately battle-tested against a very complex contract, and, except for some minor issues, it met our needs.
Since that time, we have used the Lorentz documentation capabilities in many projects, improving it little-by-little in parallel.
Recent changes include the addition of a table of contents and git revisions + some cosmetic changes and fixes.

You can find examples of production-ready documentation in the [morley-ledgers repository](https://gitlab.com/morley-framework/morley-ledgers/-/tree/autodoc/master/autodoc), see e.g. [`ManagedLedger`'s documentation](https://gitlab.com/morley-framework/morley-ledgers/-/blob/autodoc/master/autodoc/ManagedLedger.md).

Indigo, as a high-level language over Lorentz, inherits all the methods for working with documentation.
For further reading on how to use the documentation machinery, we refer the reader to the [Indigo tutorial](https://indigo-lang.gitlab.io/contract-docs/).

Thanks for staying with us!

(^≗ω≗^)

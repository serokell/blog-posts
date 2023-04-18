# Introduction

This is the second part of our series on optimizing Haskell code for [Runtime Verification](https://runtimeverification.com/) series. If you haven't had a chance to read the first part yet, you can find it [here](https://serokell.io/blog/optimizing-k-framework).

In this article, we will continue to describe the implementation of the solutions to the problems described in the previous article. Specifically, we will cover the completion of monomorphization and specialization, as well as the solution to the hashing performance issue in the `Simplifier` cache.

## Monomorphization and specialization of Simplifier

In the first part, we discussed simplifying the codebase of the K Framework by unifying `SMT` and `NoSMT`. In this part, we continue the optimization process by removing the `SimplifierT` monad transformer and replacing it with a concrete `Simplifier` monad that is built on top of the new `SMT`. This simplification enables us to enhance performance by specializing and monomorphizing the `MonadSimplify simplifier` for a specific monad.

### Simplifier

Previously, the `Simplifier` was implemented using a monad transformer, `SimplifierT smt a`, which was parameterized by the SMT solver type. The definition looked like this:

```hs
newtype SimplifierT smt a = SimplifierT (StateT SimplifierCache (ReaderT (Env (SimplifierT smt)) smt) a)

type Simplifier = SimplifierT SMT
```

The `smt` type variable could be instantiated to either `SMT` or `NoSMT`, depending on whether an external SMT solver was available. However, since `SMT` and `NoSMT` have been unified, we could now define the `Simplifier` as a base monad without the transformer:

```hs
newtype Simplifier a = Simplifier (StateT SimplifierCache (ReaderT Env SMT) a)
```

This new definition is simpler and allows us to remove the parameterization from `Env`.

We have also introduced a new function called `liftSimplifier` to the `MonadSimplify` class, similar to how we added `liftSMT` in the previous article:

```hs
class (MonadLog m, MonadSMT m) => MonadSimplify m where
    liftSimplifier :: Simplifier a -> m a
    default liftSimplifier ::
        (MonadTrans t, MonadSimplify n, m ~ t n) =>
        Simplifier a ->
        m a
    liftSimplifier = lift . liftSimplifier
    {-# INLINE liftSimplifier #-}

...
```

For the `Simplifier` instance of the `MonadSimplify` class, `liftSimplifier` can be defined simply as `liftSimplifier = id`. This allows us to lift `Simplifier` computations into any monad with a `MonadSimplify` instance.

### `trace-usage`

Our next objective is to monomorphize the functions that contain the `MonadSimplify` constraint by substituting concrete types for the abstract type parameter `m` satisfying `MonadSimplify`. Then we aim to specialize the functions that contain the `MonadSimplify` constraint by putting the `SPECIALIZE` pragmas for such functions where monomorphization is impossible due to multiple types being substituted for the abstract ones during the run of the executable.

To aid in this process, we used a tool called [trace-usage](https://github.com/serokell/trace-usage), which was mentioned in [Findings](https://serokell.io/blog/optimizing-k-framework#findings) section of previous article. We created a [branch](https://github.com/runtimeverification/haskell-backend/tree/breakerzirconia/traceusage), where every function with `MonadSimplify` in its type signature is wrapped in `Debug.Trace.Usage.traceUsage`. The branch has been utilized to generate reports on what concrete types had been substituted when running the program.

The reports are structured as lines that look like this: `N Module.function Type`, where `N` represents the number of times the type `Type` has been instantiated when running the function `Module.function`. These reports provide insights into whether we can monomorphize the functions (if only one type is being instantiated) or specialize them (if at least two types are being instantiated). In the latter case, we need to select the types instantiated the most and place `SPECIALIZE` pragmas with the type signatures including the selected types.

The report generated using traceUsage revealed some very weird type instantiations, such as this example:
```
2 Kore.Builtin.Bool.unifyBoolAnd: UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (UnifierT (LogicT Simplifier)))))))))))))))))))))))))
```
We found that a pattern of heavily nested `UnifierT` types appears in almost every function that uses the `MonadUnify` constraint, which is essentially a combination of `MonadSimplify` and [`MonadLogic`](https://hackage.haskell.org/package/logict-0.8.0.0/docs/Control-Monad-Logic-Class.html#t:MonadLogic). The definition of UnifierT is as follows:

```hs
newtype UnifierT (m :: Type -> Type) a = UnifierT
    { getUnifierT ::
        ReaderT
            (ConditionSimplifier (UnifierT m))
            (LogicT m)
            a
    }
```

The `MonadUnify` class has no methods and is defined as:

```hs
class (MonadLogic unifier, MonadSimplify unifier) => MonadUnify unifier
```
The main purpose of `MonadUnify` is to abstract over a combination of `MonadLogic` and `MonadSimplify` constraints, and provide an error-handling mechanism for unification errors using `ExceptT` over `UnificationError` within a `Simplifier` monad.

### Nested `UnifierT`

To investigate the root cause of the `UnifierT` nesting, we [created](https://github.com/runtimeverification/haskell-backend/tree/nested-unifiers) a closed type family `NotUnifierT`:

```hs
type family NotUnifierT (m :: Type -> Type) :: Constraint where
    NotUnifierT (UnifierT m) = TypeError ( 'Text "Should not be UnifierT" ':$$: 'ShowType (UnifierT m))
    NotUnifierT _ = ()
```
This type family takes a type parameter `m` and returns a trivial constraint that is always satisfied if `m` is not `UnifierT`. However, if `m` is `UnifierT`, it returns a custom `TypeError` with the message "Should not be UnifierT" and the actual type that caused the error. 

For more on type families in Haskell, check out our blog post: [Type Families in Haskell: The Definitive Guide](https://serokell.io/blog/type-families-haskell).

We added a `NotUnifierT m` constraint to the `MonadUnify (UnifierT m)` instance like this:

```hs
instance (MonadSimplify m, NotUnifierT m) => MonadUnify (UnifierT m)
```

This allows us to catch the situation where at least one level of nesting is present. If `m` is `UnifierT`, then we're working in `UnifierT (UnifierT n)`, which triggers our custom type error.

To address the circular dependencies caused by the imports of `NotUnifierT` in certain modules, we split the relevant declarations and definitions between the `hs-boot` files for [`Kore.Unification.UnifierT`](https://github.com/runtimeverification/haskell-backend/blob/0ff877eb7967310650ac92b5013c7a0a4bffcdd6/kore/src/Kore/Unification/UnifierT.hs-boot) and [`Kore.Unification.NotUnifierT`](https://github.com/runtimeverification/haskell-backend/blob/0ff877eb7967310650ac92b5013c7a0a4bffcdd6/kore/src/Kore/Unification/NotUnifierT.hs-boot). Then we used `{-# SOURCE #-}` imports to these modules to break the circular dependencies in relevant places.

After adding the constraint to the relevant type signatures, we encountered the first instance of the custom type error in `Kore.Simplify.Not.notSimplifier`. The `notSimplifier` is defined as follows:

```hs
notSimplifier :: MonadSimplify simplifier => NotSimplifier simplifier
notSimplifier = NotSimplifier simplify
```
Here, `simplify` is a function used to simplify `Not` pattern in `Kore` language.

We observed that the `notSimplifier` was used in multiple places, and one such instance was in the `termEqualsAnd` function:

``` hs
termEqualsAnd ::
    HasCallStack =>
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    MaybeT (LogicT Simplifier) (Pattern RewritingVariableName)
termEqualsAnd p1 p2 =
    MaybeT $ run $ maybeTermEqualsWorker p1 p2
  where
    run it =
        (runUnifierT Not.notSimplifier . runMaybeT) it
            >>= Logic.scatter

    maybeTermEqualsWorker ::
        forall unifier.
        MonadUnify unifier =>
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        MaybeT unifier (Pattern RewritingVariableName)
    maybeTermEqualsWorker =
        maybeTermEquals Not.notSimplifier termEqualsAndWorker

    termEqualsAndWorker ::
        forall unifier.
        MonadUnify unifier =>
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        unifier (Pattern RewritingVariableName)
    termEqualsAndWorker first second =
        scatterResults
            =<< runUnification (maybeTermEqualsWorker first second)
      where
        runUnification = runUnifierT Not.notSimplifier . runMaybeT
        scatterResults =
            maybe
                (return equalsPattern)
                Logic.scatter
                . sequence
```

The function `runUnification`, which is defined in a where block of `termEqualsAndWorker`, has a type signature of `MaybeT (UnifierT unifier) a -> unifier [a]`. In this signature, `unifier` is constrained by `MonadUnify` in the `termEqualsAndWorker`. The only available instance of `MonadUnify` is `UnifierT m`, meaning that argument to `runUnification` must have at least one redundant `UnifierT`. Additionally, `runUnification` is called inside of `termEqualsAndWorker`, which is mutually recursive with `maybeTermEqualsWorker`. This mutual recursion was the main source of the nesting of `UnifierT`.

We [replaced](https://github.com/runtimeverification/haskell-backend/pull/3380) the `runUnification` function with `runMaybeT` in `termEqualsAndWorker`, and made a slight modification to `scatterResults` to match this change:

```hs
scatterResults = pure . fromMaybe equalsPattern . Logic.scatter
```

After implementing the change, we used our `trace-usage` utility and found that most of the nested `UnifierT`s had been removed. However, some functions still had an extra level of `UnifierT`.

We revisited the `notSimplifier` and traced its usage to identify the last remaining redundant layer of `UnifierT`. We observed two things. Firstly, the `runUnifierT` function takes `notSimplifier` as an argument of type `MonadSimplify m => NotSimplifier (UnifierT m)`:

```hs
runUnifierT ::
    MonadSimplify m =>
    NotSimplifier (UnifierT m) ->
    UnifierT m a ->
    m [a]
runUnifierT notSimplifier = observeAllT . evalEnvUnifierT notSimplifier

```
Secondly, we noticed that the simplify function, used to define `notSimplifier`, eventually calls the `termAnd` function:

```hs
termAnd ::
    forall simplifier.
    MonadSimplify simplifier =>
    HasCallStack =>
    NotSimplifier (UnifierT simplifier) ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    LogicT simplifier (Pattern RewritingVariableName)
termAnd notSimplifier p1 p2 =
    Logic.scatter
        =<< (lift . runUnifierT notSimplifier) (termAndWorker p1 p2)
  where
    termAndWorker ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        UnifierT simplifier (Pattern RewritingVariableName)
    termAndWorker first second =
        maybeTermAnd notSimplifier termAndWorker first second
            & runMaybeT
            & fmap (fromMaybe andPattern)
      where
        andPattern = Pattern.fromTermLike (mkAnd first second)
```

Here, we also call `runUnifierT`, which requires `NotSimplifier (UnifierT simplifier)`. However, in this case, simplifier is already specified as `MonadSimplify m => UnifierT m`. If we substitute this in the type signature of `runUnifierT`, we get:

```hs
runUnifierT :: MonadSimplify m => NotSimplifier (UnifierT (UnifierT m)) -> UnifierT (UnifierT m) a -> UnifierT m [a]
```

Therefore, `termAndWorker` has the type:

```hs
termAndWorker ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    UnifierT (UnifierT simplifier) (Pattern RewritingVariableName)
```

In this case, `runUnifierT` removes the outer `UnifierT`, and the inner `UnifierT` belongs to `notSimplifier`, which calls this function.

To address this issue, we opted to [eliminate `UnifierT` from `NotSimplifier`](https://github.com/runtimeverification/haskell-backend/pull/3404) in the `runUnifierT` function, resulting in the following type signature:

```hs
runUnifierT :: MonadSimplify m => NotSimplifier m -> UnifierT m a -> m [a]
```

As a result of this modification, we needed to remove the `UnifierT` type variable from the definition of `NotSimplifier` in all other functions where `notSimplifier` was passed as an argument. Additionally, we [changed](https://github.com/runtimeverification/haskell-backend/pull/3387) `NotSimplifier` from a data type to a class to avoid the need for manual passing.

After implementing the changes, the `trace-usage` tool showed that there were no longer any nested `UnifierT` types in our code. This enabled us to monomorphize and specialize the `simplifier` type variables, which can improve performance. The compiler can now inline code of this functions, leading to additional optimizations.

### Monomorphization and specialization

We used the `trace-usage` report to identify type variables that could be replaced with concrete types if only one type was instantiated during execution. For those whose type variables were instantiated to multiple different types, we added `SPECIALIZE` pragmas. We found that most functions could be monomorphized, and only a few required specialization. The process of monomorphization and specialization was divided into five pull requests: [part 1](https://github.com/runtimeverification/haskell-backend/pull/3336), [part 2](https://github.com/runtimeverification/haskell-backend/pull/3346), [part 3](https://github.com/runtimeverification/haskell-backend/pull/3375), [part 4](https://github.com/runtimeverification/haskell-backend/pull/3408), and [part 5](https://github.com/runtimeverification/haskell-backend/pull/3414).

During this process, we also encountered cases of nested `MaybeT` and `ReaderT` caused by redundant `maybeT` and `runReaderT` calls, which added an unnecessary transformer layer in the argument. We resolved these issues by removing these redundant calls, which were straightforward solutions. We implemented these fixes in the pull requests titled [Remove nested ReaderT](https://github.com/runtimeverification/haskell-backend/pull/3418) and [Remove nested MaybeT](https://github.com/runtimeverification/haskell-backend/pull/3419).

### Summary

In this step, we simplified the types in codebase by introducing a concrete `Simplifier` monad to replace the `SimplifierT` transformer. This was possible because we had previously unified `SMT` and `NoSMT`. We also addressed the issues with nested transformers, which allowed us to monomorphize most of the type variables that were constrained by `MonadSimplify`. For functions not suitable for monomorphization, we added `SPECIALIZE` pragmas. This simplified the types and resulted in a speedup of approximately 15%.

In addition, the `trace-usage` report indicated that all type variables constrained by `MonadUnify` were instantiated to `UnifierT Simplifier`. However, we cannot monomorphize these type variables as it would create circular dependencies. Nonetheless, it is planned to replace `UnifierT` with a new implementation of the concrete `Unifier` monad on top of `Simplifier`, which will have a more principled implementation of the unification algorithm. The fact that all type variables constrained by `MonadUnify` are instantiated to `UnifierT Simplifier` is a positive indication towards this replacement.

## Hashing problem

As we described in our [previous blog post](https://serokell.io/blog/optimizing-k-framework#cache-hit-ratio), although the cache was performing well with a high hit ratio, the lookup function was causing a bottleneck due to the time spent on hashing and comparing keys. To address this issue, there were two potential solutions: switch to a different data structure that avoids hashing and comparisons entirely or try to minimize the amount of time required to hash and compare keys by micro-optimizing the respective operations.

### `SimplifierChache`

Let's first take a look at what the cache looks like. `SimplifierCache` is a HashMap cache used to keep track of equations that have already been attempted but failed to apply:

```hs
newtype SimplifierCache = SimplifierCache
    { attemptedEquationsCache ::
        HashMap
            EvaluationAttempt
            (AttemptEquationError RewritingVariableName)
    }
```

The `EvaluationAttempt` data type is used as the key in the HashMap. It's defined as a pair of an equation and a term:

```hs
data EvaluationAttempt = EvaluationAttempt
    { cachedEquation :: Equation RewritingVariableName
    , cachedTerm :: TermLike RewritingVariableName
    }
    deriving stock (Eq, Ord)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable)
```

The `Equation` data type is utilized to represent equational rules in `K` , which may take the form of either function definition or simplification rules. Its definition is as follows:

```hs
data Equation variable = Equation
    { requires :: !(Predicate variable)
    , argument :: !(Maybe (Predicate variable))
    , antiLeft :: !(Maybe (Predicate variable))
    , left :: !(TermLike variable)
    , right :: !(TermLike variable)
    , ensures :: !(Predicate variable)
    , attributes :: !(Attribute.Axiom Symbol variable)
    }
```

The `TermLike` data type serves as the internal representation of patterns, especially terms, in the `Kore` language. Under the hood, `TermLike` is essentially the `Cofree` data type from the [`free`](https://hackage.haskell.org/package/free-5.2) package. However, rather than defining a `newtype` over `Cofree`, `TermLike` is defined inline for performance reasons. This performance boost comes from the fact that the instances of the `Recursive` and `Corecursive` classes from the [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes) package correspond to unwrapping and wrapping the newtype, respectively, which is done for free at runtime. 

The definitions of `TermLike`, `Recursive`, and `Corecursive` are as follows:

```hs
newtype TermLike variable = TermLike
    { getTermLike ::
        CofreeF
            (TermLikeF variable)
            (TermAttributes variable)
            (TermLike variable)
    }

instance Recursive (TermLike variable) where
    project = getTermLike
    {-# INLINE project #-}

instance Corecursive (TermLike variable) where
    embed = TermLike
    {-# INLINE embed #-}
```

`Cofree` allows us to create a data structure that is both recursive and comonadic, which can be useful for representing data with a tree-like structure, such as abstract syntax trees. The basic idea is that we can attach annotations or metadata to the nodes of the tree while preserving the underlying structure, which makes it possible to define operations that traverse the tree while also accessing the annotations.

The `TermLikeF` type is a so-called base functor that is used to represent the structure of an abstract syntax tree. Its definition looks like this:

```hs
data TermLikeF variable child
    = AndF !(And Sort child)
    | ApplySymbolF !(Application Symbol child)
    | ApplyAliasF !(Application (Alias (TermLike VariableName)) child)
    | BottomF !(Bottom Sort child)
    | CeilF !(Ceil Sort child)
    | DomainValueF !(DomainValue Sort child)
    | EqualsF !(Equals Sort child)
    | ExistsF !(Exists Sort variable child)
    | FloorF !(Floor Sort child)
    | ForallF !(Forall Sort variable child)
    | IffF !(Iff Sort child)
    | ImpliesF !(Implies Sort child)
    | InF !(In Sort child)
    | MuF !(Mu variable child)
    | NextF !(Next Sort child)
    | NotF !(Not Sort child)
    | NuF !(Nu variable child)
    | OrF !(Or Sort child)
    | RewritesF !(Rewrites Sort child)
    | TopF !(Top Sort child)
    | InhabitantF !(Inhabitant child)
    | StringLiteralF !(Const StringLiteral child)
    | InternalBoolF !(Const InternalBool child)
    | InternalBytesF !(Const InternalBytes child)
    | InternalIntF !(Const InternalInt child)
    | InternalStringF !(Const InternalString child)
    | InternalListF !(InternalList child)
    | InternalMapF !(InternalMap Key child)
    | InternalSetF !(InternalSet Key child)
    | VariableF !(Const (SomeVariable variable) child)
    | EndiannessF !(Const Endianness child)
    | SignednessF !(Const Signedness child)
    | InjF !(Inj child)
```

The `TermAttributes` type is used to represent the attributes of a pattern that are collected during verification. 

```hs
data TermAttributes variable = TermAttributes
    { termSort :: !Sort
    , termFreeVariables :: !(Attribute.FreeVariables variable)
    , termFunctional :: !Attribute.Functional
    , termFunction :: !Attribute.Function
    , termDefined :: !Attribute.Defined
    , termCreated :: !Attribute.Created
    , termSimplified :: !Attribute.Simplified
    , termConstructorLike :: !Attribute.ConstructorLike
    }
```

### Replacing the data structure

One potential solution to this issue is to replace the existing HashMap data structure with a different one. In an attempt to address the problem, we initially tried to replace it with the strict Map data structure from the [containers](https://hackage.haskell.org/package/containers-0.6.7) package, which would eliminate the need for hashing altogether. Despite the fact that this replacement eliminated the bottleneck caused by hashing, we discovered that it actually resulted in an overall performance degradation.

In our [second attempt](https://github.com/runtimeverification/haskell-backend/tree/diogo/%233133-cache-hit-ratio-trie) we opted to use the trie map data structure from the [trie-simple](https://hackage.haskell.org/package/trie-simple) package.

The trie data structure `TMap c v` is used to store a mapping from a list of characters to a value of some sort. This data structure is essentially isomorphic to the `Map [c] v` structure, but provides better querying efficiency compared to the latter.

In order to take advantage of the `TMap` data structure, we had to flatten our `EvaluationAttempt` data type manually, which is used as a key. For this purpose, we introduced a large data type called `Component` that describes every possible component of terms and predicates used in `EvaluationAttempt`:

```hs
data Component
    = CPredicate
    | CNoPredicate
    | CTermLike
    | CAxiom !(Axiom Symbol RewritingVariableName)
    | CElementalVariable !(Syntax.ElementVariable RewritingVariableName)
    | CSetVariable !(Syntax.SetVariable RewritingVariableName)
    | CSort !Sort
    | CSymbol !Symbol
    | CAnd
    | CApplySymbol
    | CApplyAlias !Syntax.Id
    | CBottom
    | CCeil
    | CDomainValue
    | CEquals
    | CExists
    | CFloor
    | CForall
    | CIff
    | CImplies
    | CIn
    | CMu
    | CNext
    | CNot
    | CNu
    | COr
    | CRewrites
    | CTop
    | CInhabitant
    | CStringLiteral !StringLiteral
    | CInternalBool !InternalBool
    | CInternalBytes !InternalBytes
    | CInternalInt !InternalInt
    | CInternalString !InternalString
    | CInternalList
    | CInternalMap !(InternalMap Key (TermLike RewritingVariableName))
    | CInternalSet !(InternalSet Key (TermLike RewritingVariableName))
    | CVariable !(Syntax.SomeVariable RewritingVariableName)
    | CEndianness !Endianness
    | CSignedness !Signedness
    | CInj !Syntax.Id
```

We also defined a function called [`flatten`](https://github.com/runtimeverification/haskell-backend/blob/4da416b83773f1b57fb83eb430855dfe3bb819ad/kore/src/Kore/Simplify/Simplify.hs#L388-L458) that maps the complex tree-like structure of the `EvaluationAttempt` data type to a flat list of `Component`s. 

Once again, we were successful in eliminating performance issues caused by hashing. However, we didn't observe any noticeable improvement in performance due to the need for flattening keys.

### Pre-hashing

To potentially address the performance issue, we considered pre-calculating the hashes and thus avoiding the repeated hashing of the same data. Initially, we opted to [pre-hash the `Equation` data type](https://github.com/runtimeverification/haskell-backend/pull/3181) by adding an additional field to store the pre-calculated hash:

```hs
data Equation variable = Equation
    { requires :: !(Predicate variable)
    , argument :: !(Maybe (Predicate variable))
    , antiLeft :: !(Maybe (Predicate variable))
    , left :: !(TermLike variable)
    , right :: !(TermLike variable)
    , ensures :: !(Predicate variable)
    , attributes :: !(Attribute.Axiom Symbol variable)
    , equationHash :: !Int
    }
```

Furthermore, we implemented a new data type called `EquationNoHash`, which mirrors the previous version of `Equation` without the `equationHash` field. To complement this, we also created a smart constructor for `Equation` that calculates the hash value of `EquationNoHash`:

```hs
{-# INLINE mkEquation' #-}
mkEquation' ::
    Hashable variable =>
    (Predicate variable) ->
    (Maybe (Predicate variable)) ->
    (Maybe (Predicate variable)) ->
    (TermLike variable) ->
    (TermLike variable) ->
    (Predicate variable) ->
    (Attribute.Axiom Symbol variable) ->
    Equation variable
mkEquation' req arg antiLeft left right ensures attrs eqHash  =
    Equation req arg antiLeft left right ensures attrs eqHash
        (hash $ EquationNoHash req arg antiLeft left right ensures attrs)
```

Thus, we could define a `Hashable` instance for `Equation` by directly accessing the `equationHash` field:

```hs
instance Hashable (Equation equation) where
    hashWithSalt _ = equationHash
    {-# INLINE hashWithSalt #-}
```

Unfortunately, pre-hashing the `Equation` data type improved the speed by only 1%. 

As the next step, we attempted to [pre-hash the `TermLike` data type](https://github.com/runtimeverification/haskell-backend/tree/diogo/%233113-cache-hit-ratio-terms). To achieve this, we added a new wrapper type called `PreHashed` to pre-hash the `TermLike` data type, and also included a smart constructor for it:

```hs
data PreHashed a = PreHashed
    { preHashed :: !a
    , preHash :: ~Int
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

mkPreHashed :: Hashable a => a -> PreHashed a
mkPreHashed !a = PreHashed a (hash a)
{-# INLINE mkPreHashed #-}
```

This type is similar to the `Data.Hashable.Hashed` type, but it has a few differences:

* It has a lazy hash.
* It has a strict `a`.
* The `Eq` instance does not use the hash (to avoid evaluating it).

This change, however, resulted in a negligible impact with only a 0.33% overall speedup.

Our [next attempt](https://github.com/runtimeverification/haskell-backend/commits/jb/caching-termlike-hashes) was to pre-compute hash for the term and store it in `TermAttributes` of `TermLike`. This pre-computed hash is then accessed by the `Hashable` instance rather than recursively traversing the `TermLikeF` data structure. To implement this, we added a new field `termHash` to `TermAttributes` to store the pre-computed hash value, just like we did for the `Equation` type:

```hs
data TermAttributes variable = TermAttributes
    { termSort :: !Sort
    , termFreeVariables :: !(Attribute.FreeVariables variable)
    , termFunctional :: !Attribute.Functional
    , termFunction :: !Attribute.Function
    , termDefined :: !Attribute.Defined
    , termCreated :: !Attribute.Created
    , termSimplified :: !Attribute.Simplified
    , termConstructorLike :: !Attribute.ConstructorLike
    , termHash :: Int -- caches the TermLikeF hash value for efficiency
    }
```

To compute the hash value using the `hashWithSalt` function, we used the pre-computed hash value stored in `termHash`, along with the provided `salt` value:

```hs
instance Hashable variable => Hashable (TermLike variable) where
    hashWithSalt salt (Recursive.project -> TermAttributes{termHash} :< _) =
        salt `hashWithSalt` termHash
    {-# INLINE hashWithSalt #-}

```

To produce the pre-computed hash value, we used the derived `Hashable` instance of `TermLikeF`, which will call hash on the respective sub-structure wrapped into one of the `TermLikeF` constructors. For all such sub-structures, the derived `Hashable` instance will then recurse into the contents, using the cached hashes directly instead of performing a deep recursion.

Although the change resulted in a 6% speedup, it also caused some integration tests to fail, and unfortunately, we were unable to find a way to fix them while keeping the pre-computed hash in `TermAttributes`.

So, to avoid storing pre-computed hash in `TermAttributes`, we [cached the hash](https://github.com/runtimeverification/haskell-backend/pull/3338) of the `TermLikeF` in the `TermLike` itself and extracted it in the `Hashable` instance instead of recursively traversing into the `TermLikeF`. To achieve this, we modified the `TermLike` definition:

```hs
data TermLike variable = TermLike__
    -- Some fields below are lazy to better match Cofree.
    { _tlAttributes :: ~(TermAttributes variable)
    , -- | A hash of _tlTermLikeF
      _tlHash :: ~Int
    , _tlTermLikeF :: ~(TermLikeF variable (TermLike variable))
    }
```
We replaced the `CofreeF` type with a record that includes the `_tlHash` field storing the pre-computed hash of `_tlTermLikeF`. 

Here are the updated `Hashable`, `Recursive`, and `Corecursive` instances for the modified `TermLike`:

```hs
instance Eq variable => Hashable (TermLike variable) where
    hashWithSalt salt (TermLike__ _ hsh _) =
        salt `hashWithSalt` hsh
    {-# INLINE hashWithSalt #-}

instance Recursive (TermLike variable) where
    project = getTermLike
    project (TermLike__ attr _hash pat) = attr :< pat
    {-# INLINE project #-}

instance Corecursive (TermLike variable) where
    embed = TermLike
instance Hashable variable => Corecursive (TermLike variable) where
    embed (attr :< pat) = TermLike__ attr (hash pat) pat
    {-# INLINE embed #-}
```

In addition, a `Lens'` was added for editing the `TermAttributes` of a `TermLike`, which is more efficient than using `project` followed by `embed` because it avoids recomputing the hash on `embed`:

```hs
termLikeAttributes :: Lens' (TermLike variable) (TermAttributes variable)
termLikeAttributes =
    Lens.lens
        (\(TermLike__ attrs _ _) -> attrs)
        ( \(TermLike__ _ hsh termLikeF) attrs ->
            TermLike__ attrs hsh termLikeF
        )
```

### Summary

To address this problem, we used pre-computation of the hash value to reduce the computational cost of computing the hash. In the case of the modified `TermLike` data structure, we pre-computed the hash value of the `TermLikeF` component and used it in the `Hashable` instance to avoid recomputing the hash. We also modified the `Recursive` and `Corecursive` type class instances to account for the pre-computed hash field in `TermLike`. In total, the changes above improve performance by approximately 10%.

## Conclusion

In the second phase of our collaboration with Runtime Verification, we finished work on the problems described in the first article.

In the second part of the monomorphization and specialization task, we replaced `SimplifierT` with a concrete `Simpliter` monad. This was possible because we had unified `SMT` and `NoSMT` before. We also addressed the issues with nested transformers, which allowed us to monomorphize most of the type variables that were constrained by `MonadSimplify`. This not only simplified the types but also resulted in a 15% speedup. Moreover, we also cleaned up the code in preparation for the future replacement of the Unifier implementation.

The other task involved optimizing the cache by pre-computing the hash of keys. We modified the `TermLike` data type to include a pre-computed hash value of `TermLikeF`. This reduced the time spent on computing hash values, which resulted in a performance improvement of approximately 10%. 

To ensure you don't miss any of our future blog posts, stay updated by following us on [Twitter](https://twitter.com/serokell) or subscribe to our mailing list using the form below.




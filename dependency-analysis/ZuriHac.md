# Dependency Analysis of Haskell Declarations

Most languages require the programmer to declare functions and data types before their use. For example, the following code is not a valid C or C++ program:

```c
int main() {
  f();       // error: use of undeclared identifier 'f'
}

void f();    // ... even though it’s declared down here.
```

Among the languages that have this restriction are Python, JavaScript, Rust, and even some functional ones, for example OCaml.

Haskell is one of the few languages that let you structure your program how you see fit. Functions and data types can be declared and used in arbitrary order:

```haskell
main = putStrLn str
str = "Hello, World"
```

Declarations out of dependency order are a challenge to the compiler, though. How can GHC know if `putStrLn str` in `main` is well-typed without knowing the type of `str`? It needs to check `str` first, even though it comes second.

That’s where **dependency analysis** comes into play. Before type checking a module, GHC rearranges its declarations in such a way that functions and data types are declared before their use. Then it can process them top-down, just as compilers for other languages do.

Dependency analysis can handle fairly convoluted programs:

```haskell
data Nat = Z | S Nat

parity :: Nat -> Parity
parity Z = Even
parity (S n) = flipP (parity n)

data Parity = Even | Odd

flipP :: Parity -> Parity
flipP Even = Odd
flipP Odd = Even
```

In this example, both `Parity` and `flipP` are used before they are declared. But dependency analysis determines that the order of processing should be: `Nat`, `Parity`, `flipP`, `parity`.

## Type-checking Environment

Why does the type checker expect declarations to be in any particular order? This is not merely an engineering issue, it has its roots in theory. Specifically, type theorists often define **typing contexts** as follows:

$ \Gamma ::= \varnothing \mid \Gamma, x : \tau $

If you're not a fan of mathematical notation and greek letters, no worries: this basically means we have an ordered list of pairs `[(Name, Type)]`. And whenever we say that an expression is well-typed, we imply that it is well-typed in a given context $\Gamma$.

For example, can you say if `x + 1` is well-typed? Not without knowing the type of `x`, so the context $\Gamma$ must have an entry such as `x : Int` or `x : String`. In other words, the formal definition of a type system takes it for granted that when we type check an expression, we already have all of its dependencies in the typing context.

That is also how GHC operates: there's the notion of a **type-checking environment**, quite similar to the notion of a typing context. When we type check a definition, we add it to the environment, and all subsequent definitions can refer to it.

## Dependency Analysis: a Bird's Eye View

The first step of the dependency analysis is to separate types from terms. Currently, types can (almost) never depend on terms, so types are always checked before terms. This, of course, will have to change in the future if we want to implement dependent types, but that is out of the scope of this article.

1. Type-level declarations: `Nat(Z, S)`, `Parity(Even, Odd)`
2. Term-level declarations: `parity`, `flipP`

So even before we start looking at the dependencies, we know that `Nat` and `Parity` will be checked before `flipP` and `parity`.

In this article, We will primarily focus on the type-level declarations, as that is where we have some open issues and ongoing development. But first a small detour into Template Haskell, which, turns out, blurs the line between terms and types in an unexpected way.

### Template Haskell splices

Above I've mentioned that types can never depend on terms, but there is one exception to this, and that is Template Haskell splices. Let's take a look at this example:

```haskell
boodDesc =
  [("id", [t| UUID |])
  ,("title", [t| Text |])
  ,("author", [t| Text |])

$(generateRecord "Book" 'bookDesc)
-- data Book = Book { bookId :: UUID, bookTitle :: Text, bookAuthor :: Text }

type Library = [Book]
```

Here we have a Template Haskell function `generateRecord` that takes a _term-level list_ of fields and uses it to generate a _record data type_. The generated type depends on a term: if `bookDesc` is changed, `Book` will also change. So how does GHC do this if terms are always checked after types?

Before GHC does any type-checking or name resolution in a module, it splits that module into groups (called `HsGroup`). Each group is separated by a Template Haskell splice:

![enter image description here](https://i.imgur.com/9UtYb4E.png)

Then those groups are analyzed and type-checked individually in the order they appear in the source. After a group is type-checked, all of its declarations are added to the type-checking environment, then Template Haskell code is executed, and then GHC moves on to the next group.

A side-effect of this splitting behavior is that you can't refer to declarations that come after a Template Haskell splice:

```haskell
boodDesc =
  [("id", [t| UUID |])
  ,("title", [t| Text |])
  ,("format", [t| FileFormat |]) -- FileFormat is not in scope
```

```haskell
$(generateRecord "Book" 'bookDesc)
-- data Book = Book { bookId :: UUID, bookTitle :: Text, bookAuthor :: Text }

type Library = [Book]
data FileFormat = EPUB | PDF
```

This is because the group with `bookDesc` will be fully processed first, and only then GHC will move on to the group with `FileFormat`.

In the absence of Template Haskell splices, the entire module constitues a single `HsGroup`.

## Dependency Analysis at the Type Level

So within a given `HsGroup`, after separating types and terms, how does GHC figure out the order in which to put declarations before type checking them?

Let's look at this example and see step-by-step what happens:

```haskell
data L =
  Result ListCB | Operation Op

data Op =
  Reverse L | Sort L

data ListCB =
  NilCB | ConsCB Char ListBC

data ListBC =
  NilBC | ConsBC Bool ListCB
```

The first thing that GHC does is go through each declaration and make a list of names that the declaration mentions:

| Declaration |  Mentioned names |
|-------------|------------------|
| `L`         | `ListCB`, `Op`   |
| `Op`        | `L`              |
| `ListCB`    | `Char`, `ListBC` |
| `ListBC`    | `Bool`, `ListCB` |

Then GHC builds a directed graph using declarations as vertices and mentioned names as edges. For example, we can see that `L` mentions `ListCB` and `Op`, so it has edges to those vertices in the graph:

![](https://i.imgur.com/0poEq5O.png)

After building the graph, GHC will find strongly connected components. A [strongly connected component](https://en.wikipedia.org/wiki/Strongly_connected_component) (or SCC for short) is a part of a directed graph where each vertex is reachable from every other vertex. Or, in our case, it is a mutually recursive group. You can think of finding SCCs as building a "supergraph" where each vertex now is a mutually recursive group and edges are dependencies between the groups. So in the example, above we have two mutually recursive groups: `ListCB` with `ListBC` and `L` with `Op`. And the `{L, Op}` group depends on the `{ListCB, ListBC}` group.

And finally, after finding SCCs, GHC puts them in topological order:

```haskell
-- Group 1
data ListCB = NilCB | ConsCB Char ListBC
data ListBC = NilBC | ConsBC Bool ListCB
```

```haskell
-- Group 2
data L  = Result ListCB | Operation Op
data Op = Reverse L | Sort L
```

### Type family instances

Type family instances, unlike other sorts of declarations, don't have names, and nothing can directly refer to them. Thus they're not a part of the graph and are handled differently. Let's take a look:

```haskell
type family PropType a

data WTitle
data WResizable

type MainWindow :: forall prop -> PropType prop
type family MainWindow prop where
  MainWindow WTitle = "Text Editor"
  MainWindow WResizable = True

type instance PropType WTitle = Symbol
type instance PropType WResizable = Bool
```

After all of the graph and SCC business is done, we'll get the following groups:

```haskell
-- Group 1
type family PropType a
```

```haskell
-- Group 2
data WTitle
```

```haskell
-- Group 3
data WResizable
```

```haskell
-- Group 4
type MainWindow :: forall prop -> PropType prop
type family MainWindow prop where
  MainWindow WTitle = "Text Editor"
  MainWindow WResizable = True
```

But where do the type instances go?

For every type instance, GHC also builds the list of names it mentions, just like it does for other sorts of declarations. But the instance itself has no name, so it does not participate in topological sorting. Instead, it is added to the earliest group where all of its dependencies are satisfied.

The target group is determined by going through each group one by one and removing its declarations from the list of remaining dependencies of the instance. Once the list is empty, it means all of the instance dependencies have been defined in the current and prior groups, so the instance can be inserted into the current group.

| Instance             | Dependencies                     |
|----------------------|----------------------------------|
|`PropType WTitle`     | `PropType`, `WTitle`, `Symbol`   |
|`PropType WResizable` | `PropType`, `WResizable`, `Bool` |

So in our example, first `Symbol` and `Bool` will be removed from the list since they're imported. This leaves us with the following:

| Instance             | Dependencies (remaining)   |
|----------------------|----------------------------|
|`PropType WTitle`     | `PropType`, `WTitle`       |
|`PropType WResizable` | `PropType`, `WResizable`   |

Then GHC will process the first group, the one that defines `PropType`, and remove `PropType` from dependencies of both instances. So then we have:

| Instance             | Dependencies (remaining)   |
|----------------------|----------------------------|
|`PropType WTitle`     | `WTitle`                   |
|`PropType WResizable` | `WResizable`               |


Next it will get to the group with `WTitle`. `WTitle` will be removed from the list of dependencies, and since it's the last dependency of `PropType WTitle`, the instance is inserted into that group:

```haskell
-- Group 2
data WTitle
type instance PropType WTitle = Symbol  -- inserted
```

And then the same thing happens on the next group with `PropType WResizable`, making our final groups look like this:

```haskell
type family PropType a
```

```haskell
data WTitle
type instance PropType WTitle = Symbol
```

```haskell
data WResizable
type instance PropType WResizable = Bool
```

```haskell
type MainWindow :: forall prop -> PropType prop
type family MainWindow prop where
  MainWindow WTitle = "Text Editor"
  MainWindow WResizable = True
```

And this algorithm works pretty well. However, it is not failproof. Let's take a look at an example where it falls short!

### Current algorithm's shortcoming

#### Example I: `Open`

```haskell
type family Open a
type instance Open Bool = Nat
type instance Open Char = F Float
type instance Open Float = Type

type F :: forall a -> Open a
type family F a
type instance F Bool = 42 -- :: Open Bool (~ Nat)
type instance F Char = '[0, 1] -- :: Open Char (~ F Float ~ [Nat])
type instance F Float = [Nat] -- :: Open Float (~ Type)
```

There's a lot to unpack here, and we'll start with the `Open` type family.  At its core, it's a very simple type family: it just maps some `Type` into some other `Type`, like `Open Bool` equals to `Nat`. `Open Char` is more interesting because it equals to `F Float`, so let's look at `F`.

`F` takes some type `a` and should return something of kind `Open a`. For example, `F Bool` should return something with kind `Open Bool`. Since `Open Bool` is `Nat`, it should return `Nat` as it does.

`F Char` is the trickiest one. It should return something of kind `Open Char`, but `Open Char` is `F Float`. If we look at `F Float`, we'll see it reduces to `[Nat]`, so in the end `F Char` should return something with kind `[Nat]` as it does.

So this code is correct, everything should type check here. However, if we try to compile that code in current GHC, we'll get this error:

```
• Expected kind ‘Open Char’,
  but ‘'[0, 1]’ has kind ‘[Nat]’
```
So GHC complains that `Open Char` is not `[Nat]`, but we know that it is. Why doesn't GHC know that? That's because of how instances are grouped with the current algorithm:

```haskell
type family Open a
type instance Open Bool = Nat
type instance Open Float = Type
```

```haskell
type F :: forall a -> Open a
type family F a
type instance Open Char = F Float
type instance F Bool = 42
type instance F Char = '[0, 1]
type instance F Float = [Nat]
```

Notice how all of the `F`'s instances as well as `Open Char` end up in the same group. This is a problem because to kind check `F Char`, we need to know what `Open Char` and `F Float` reduce to, or in other words, those instances should already be in the type-checking environment. But since they're part of the same mutually recursive group, they are checked at the same time, and neither of them is in the type-checking environment yet.

#### Example II: `IxKind`

To better showcase the problem, let's take a look at another example that exhibits the same issue:

```haskell
type family IxKind (m :: Type) :: Type
type family Value (m :: Type) :: IxKind m -> Type
data T (k :: Type) (f :: k -> Type) = MkT

type instance IxKind (T k f) = k
type instance Value (T k f) = f
```
Here we have two type families:

* `IxKind` is fairly simple, just maps a `Type` to some other `Type`
* `Value` takes some type `m` and returns a function from `IxKind m` into `Type`

And then there's a data type `T` in which the kind of its second parameter depends on the first parameter.

Now to expose the problem, let's write out kinds explicitly in the `Value (T k f)` instance:

```haskell
type instance Value (T k f) = f -- :: k              -> Type, from the signature of T
           -- Value (T k f)        :: IxKind (T k f) -> Type, from the signature of Value
```

So as we can see, we're expected to return `IxKind (T k f) -> Type`, but we're returning `k -> Type`. So to type check this, we need to know whether `k ~ IxKind (T k f)`. And if we look at the `IxKind (T k f)` instance, we'll see that this is indeed the case. But in order to make use of this information, the `IxKind` instance must be added to the type-checking environment *before* the `Value` instance. Alas, they end up in the same group:

```haskell
type family IxKind (m :: Type) :: Type
```

```haskell
type family Value (m :: Type) :: IxKind  m -> Type
```

```haskell
data T (k :: Type) (f :: k -> Type) = MkT
type instance IxKind (T k f) = k
type instance Value (T k f) = f
```

Currently, there's no mechanism by which dependency analysis would detect that the `Value` instance depends on the `IxKind` instance.

### How do other languages solve this?

At this point, one might wonder: we have these advanced languages like Idris or Agda that are already dependently typed. How do they tackle this issue? Well, the truth is that they don't have these issue at all because they just check declarations in their written order like every other language.

Could this be done in GHC? Well, yes, but actually no. Checking declarations in the order they're written in would break a lot of currently existing code and would deviate from the standard. But perhaps we could have an extension that would let us have "ordered" blocks of code (kind of the inverse of Idris' `mutual` keyword)? We could, but this approach is considered non-Haskelly.

### The TH splices workaround

If you recall from earlier, Template Haskell splices split the code into groups that are checked strictly in order. We can use this to our advantage to kind of implement the non-Haskelly approach today.

```haskell
type family Open a
type instance Open Bool = Nat
type instance Open Float = Type
```

```haskell
$(return [])
type F :: forall a -> Open a
type family F a
type instance Open Char = F Float
type instance F Bool = 42
type instance F Float = [Nat]
```

```haskell
$(return [])
type instance F Char = '[0, 1]
```

Here, by splitting the groups properly, we force `Open Char` and `F Float` to be type checked before GHC gets to type check `F Char`, making this code compile.

But that's hacky. So how can we improve the current algorithm so that we don't have to do this?

## The `:sig` and `:def` notation

More often than not, depending on an entire declaration is too strong of a dependency. For example, to type check `data T = MkT Bool`, we need to know `Bool`'s kind (which is `Type`) but not much else. We don't care what constructors `Bool` has or anything else about it. So really, this declaration should only depend on _signature_ of `Bool` and shouldn't depend on its _definition_.

Introducing `:sig` and `:def` notation, short for signature and definition, respectively. You can think of signature as the left-hand of a declaration and of definition as the right-hand side of a declaration.

```haskell
data Bool = True | False
--   ^^^^   ^^^^^^^^^^^^
-- Bool:sig    Bool:def
```

```haskell
type family F a; type instance F Int = Bool;
                 type instance F Bool = Int;
--        ^^^^^                ^^^^^^^^^^^^
--        F:sig                   F:def
```

For open type families, all instances are considered to be a part of the same `:def` block and are grouped together. Instead of having declarations as vertices, our directed graph will have two types of vertices: `:sig`s and `:def`s. And now that we have different kinds of vertices, we need new rules for inferring edges (dependencies):

1. Mentioning the name of a type-level thing adds a dependency to that thing's `:sig`. For example, `F:def` above depends on `Bool:sig`
2. Each `:def` depends on its corresponding `:sig`.
3. Mentioning the name of a promoted data constructor adds a dependency to the parent's `:def` of that data constructor. For example, mentioning `'False` adds a dependency to `Bool:def`.
4. If a data type has no explicit kind, add a dependency from its `:sig` to its `:def`

Why do we need that last rule? Let's take a look at this simple data type:

```haskell
data T a = MkT a
```

Now if we were to split `:sig` from `:def` here, it could happen that GHC will type-check `:sig` before type-checking `:def`. Let's see what happens then:

```haskell
type T a      -- GHC will infer the most general type here:
              -- T a :: forall k. k -> Type
```

And then it will get to type checking the `:def`:

```haskell
data T a = MkT a
```

And this is where the mismatch occurs, the definition expects `a` to have kind `Type`, since data constructors could only have things of kind `Type` in them, but GHC inferred earlier that `a` is kind-polymorphic.

So unless the user provides an explicit kind signature, we cannot split `:sig` from `:def` because that might interfere with kind inference. To solve this, we add an extra edge from `:sig` to `:def` in this case, making them a mutually recursive group, which guarantees that they'll be checked together.

#### Fixing the `IxKind` example

Now let's see what the new graph looks like for our example with `IxKind` from earlier:

```haskell
type family IxKind (m :: Type) :: Type
--          ^^^^^^^^^^^^^^^^^^^^^^^^^^
--                IxKind:sig

type family Value (m :: Type) :: IxKind m -> Type
--          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--                Value:sig

data T (k :: Type) (f :: k -> Type)   =   MkT
--   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^      ^^^^^
--                T:sig                  T:def

type instance IxKind (T k f) = k
--            ^^^^^^^^^^^^^^^^^^
--                IxKind:def

type instance Value (T k f) = f
--            ^^^^^^^^^^^^^^^^^
--                Value:def
```

![enter image description here](https://i.imgur.com/93tkq2R.png)
Looks cool! But if you pay close attention, this doesn't solve our original issue. Our original issue was that we really wanted to type-check `IxKind (T k f)` before we type-check `Value (T k f)`, but there's no dependency between them on this graph!

But compared to our previous algorithm, we can now at least _indicate_ this dependency on the graph. All we need to do now is to come up with a rule that will infer this dependency for us.

One of the first proposed rules was "for every dependency from one `:sig` to another `:sig`, also add a dependency between corresponding `:def`s". This would work in this case, but sometimes this rule infers unnecessary dependencies and sometimes it does not infer enough dependencies.

At this point, what exact set of rules to use is still somewhat an open research question. [Some other rules were proposed](https://gitlab.haskell.org/ghc/ghc/-/wikis/Type-&-Class-Dependency-Analysis#optional-improvement-weak-vs-strong-edges), but we haven't tried them yet because there's a more pressing issue at hand.

### Oops! All panics!

The nature of this new algorithm is to split signatures from definitions. Let's look at this code:

```haskell
type T :: Type
data E = MkE T
data T
```

After building the graph, finding SCCs, etc., we'll get the following groups:

```haskell
type T :: Type
```
```haskell
data E = MkE T
```
```haskell
data T
```

Notice how we use `T` in `E` after we type checked the `T`'s signature but before its definition. Theoretically, that should be enough, however, the type checker makes a lot of assumptions about what is available in the environment and what is not and it cannot handle a situation like that resulting in a crash.

The work on dependency analysis is currently blocked by the type checker crash issue, and it turns out fixing that issue requires some major refactoring in the type checker code-base, which is currently ongoing.

## Conclusion

This article is made after my talk (with the same name) at ZuriHac 2021, which you can watch on our [YouTube channel].

To be informed about new articles on GHC and our current work on it, don't forget to follow us on [Twitter](https://twitter.com/serokell) or [Dev](https://dev.to/serokell). You can also read more about our work by exploring the [GHC](https://serokell.io/blog/ghc) tag. 

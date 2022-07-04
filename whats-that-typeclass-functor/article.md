You might already know the `map` function for lists:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

```none
> map (\x -> x + 1) [1, 2, 3]
[2, 3, 4]
```

It takes a function of type `a -> b` and applies it to each element of a list. The elements change, but the data type storing them (`[]`) remains the same. 

In this article, we'll call such a data type with another type inside a _context_, while the elements inside it we'll call _values_.

Transforming values inside fixed contexts like we do with `map` is common in programming. 

For example, the optional data type ([`Maybe`](https://serokell.io/blog/algebraic-data-types-in-haskell#maybe)) provides a context of a possibly failed computation. It can also be "mapped" by trying to apply a function to the wrapped value without changing the context. 

```haskell
map' :: (a -> b) -> Maybe a -> Maybe b

map' f (Just x) = Just (f x)
map' f Nothing = Nothing
```

```none
*Main> map' (\x -> x + 1) (Just 1)
Just 2
*Main> map' (\x -> x + 1) (Nothing)
Nothing
```

In Haskell, a typeclass called Functor unifies these kinds of transformations and provides common functionality for them. 

After reading this article, you will know:

- what Functor is in Haskell;
- how to define and use your own Functor instances;
- why and where functors are useful.

We'll also provide a set of exercises to consolidate your knowledge.

## How to generalize `map`

Look at the type signature of `map`:

```haskell
map :: (a -> b) -> [a] -> [b]
```

It takes a function `a -> b`. Then, it uses that function to turn its second argument (of type `[a]`) into a list of `b` — `[b]`.

Now imagine we’re using the same type of function — `a -> b` — to change the contents of `Maybe`. For now, it doesn’t matter how exactly we’d do it. What type would such a `map'` function have?

<details>
<summary>The type of <code>map'</code> modifying <code>Maybe</code></summary>

```haskell
map' :: (a -> b) -> Maybe a -> Maybe b
```
<hr>
</details>

Finally, assume there is a data type `f` with one type argument `a` — `f a`. This data type is not arbitrary, cause `map'` implementation is data-type-specific, thus it has a constraint — `Functor f`. In terms of type signature, it means that there is a prefix `Functor f =>`. We won't delve into details now, as it's the matter of the next section. So by analogy, can you figure out how the `map'` type would look like for `f a`?

<details>
<summary>The type of <code>map'</code> modifying <code>f</code></summary>

```haskell
map' :: Functor f => (a -> b) -> f a -> f b
```
<hr>
</details>

Great! This `map'` is called `fmap` or `(<$>)` in Haskell.

Here’s how it works: `fmap` takes an  `a -> b` function and an `f a` data type (`a` wrapped in a context `f`), then the function is applied to what’s inside the context, and finally, a value of type `b` wrapped in `f` is returned.

Therefore, the value changes, but the context remains the same.

Here are a few examples:

```haskell
-- apply `reverse :: String -> String` function
-- to the list of `String`s
-- to get the list of `String`s
> fmap reverse ["abc", "def", "ghi"]
["cba","fed","ihg"]
> fmap reverse []
[]

-- apply `show :: Int -> String` function
-- to the list of `Int`s
-- to get the list of `String`s
> fmap show [1..3]
["1","2","3"]
> fmap show []
[]

-- apply `(+1) :: Int -> Int` function
-- to `Maybe Int`
-- to get `Maybe Int`
> fmap (+1) $ Just 1
Just 2
> fmap (+1) Nothing
Nothing

-- apply `(> 0) :: Int -> Bool` function
-- to `Maybe Int`
-- to get `Maybe Bool`
> fmap (> 0) $ Just (-1)
Just False
> fmap (> 0) $ Just 1
Just True
> fmap (> 0) Nothing
Nothing

-- apply `chr :: Int -> Char` function
-- to pair `(a, Int)`
-- to get pair `(a, Char)`
-- Note that only the second value is being fmap'ed
> fmap chr (65,65)
(65,'A')
> fmap chr ('a',97)
('a','a')
```

As you may have guessed, `map` is just a synonym of `fmap` for list. But it can do much more.

With `fmap`, we can reverse strings, convert numbers to strings, perform algebraic and boolean operations, and do other stuff with values inside data types like `[]`, `Maybe`, `Either a`, pair `((,) a)`, and others.

Since the implementation of `fmap` is data-type-specific, to use it on a data type, that data type needs to have an implementation for it.

In Haskell, this means that the type needs to have an instance of the Functor typeclass.

## The Functor typeclass

What’s Functor? Let’s ask GHCi.

```haskell
> :info Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

Functor in Haskell is a [typeclass](https://serokell.io/blog/haskell-typeclasses) that describes two common functions — `fmap` and `(<$)`.

The kind of Functor is `(* -> *) -> Constraint`, where `* -> *` is the kind of data type which could be a Functor. You can read about kinds and the `* -> *` kind especially in our [previous article](https://serokell.io/blog/whats-that-typeclass-foldable?nocache=#generalization-of-foldr-and-foldl).

Functor provides methods for structure-preserving transformations.

You’re already familiar with `fmap`. However, just any `fmap` is not enough for a correct Functor instance. The former should follow some laws.

### Laws of Haskell typeclasses

Some of Haskell typeclasses have laws — conditions that instances have to meet. Usually, these laws come from self-titled math concept. E.g. Functor is [a mapping in the category theory](https://en.wikipedia.org/w/index.php?title=Functor&oldid=1093605134).

You may wonder why to follow these laws. The answer is simple — only if the implemented instance meets these conditions, the methods of typeclass work as expected. Otherwise, you can run into unpredictable behaviour and confuse people working with your code as well.

Checking the laws satisfiability isn't enforced by the language, hence you need to provide that yourself. It may be a bit difficult at the beginning, but after a couple of instances you will feel quite easy and natural about it.

There is one tool that could be used to verify the instance's laws, though — [QuickCheck](https://hackage.haskell.org/package/QuickCheck). It does random testing, so a property (typeclass law in our case) is checked on the large number of randomly generated values. You may look at "Checking the laws" section of [this article](https://mmhaskell.com/blog/2017/3/13/obey-the-type-laws) as an illustration.


Functor, in particular, needs `fmap` to adhere to the following laws.

---

### Laws of Functor

1. **Identity**

    `fmap id x == id x` — applying `id` function to the wrapped value changes nothing.

    Example:

    `fmap id (Just 1) == id (Just 1) == Just 1`

2. **Composition**

    `fmap f (fmap g x) == fmap (f . g) x` — applying `fmap`s sequentially is the same as applying `fmap` with the composition of functions.

    Example:

    `fmap (+1) (fmap (*2) (Just 1)) == fmap ((+1) . (*2)) (Just 1) == Just 3`


---

`fmap` is all you need to define a `Functor` instance. However, the `(<$)` operator is worth looking at too.

### The `(<$)` operator

Take a look at the type signature of `(<$)`:

```haskell
(<$) :: a -> f b -> f a
```

The operator takes a value of type `a`, a value of type `b` packed in a functor `f` – `f b` – and produces a functor `f a`. Basically, the operator packs the first argument’s value in the context of the second argument, throwing away the second value.

Now, let’s try to guess the definition of `(<$)` using the provided description and the following examples.

```haskell
-- replace an `Int` value
-- inside `Maybe Int`
-- with a `String`
> "abc" <$ Just 123
Just "abc"
> "abc" <$ Nothing
Nothing

-- replace each element
-- of the list `[Int]`
-- with an `Int`
> 1 <$ []
[]
> 1 <$ [0]
[1]
> 1 <$ [1..5]
[1,1,1,1,1]
```

Pay attention to list examples. `y <$ [x1, x2, x3]` matches `[y, y, y]` but not `[y]`, as list’s inside is many values, not a single one. So that each element of the list is transformed, but not the single value is wrapped in `[]`.


<details>
<summary> The default definition of <code>(<$)</code>.</summary>

```haskell
x <$ f = fmap (const x) f
```

<hr>
</details>

Exactly! `(<$)` just runs `fmap` with `const` function.


We got to know the basic components of Functor, and now it’s time to create our own instance of this type class!

## Creating an instance of Functor

Let’s see how you can implement your own instance of Functor. We’ll use the [`NonEmpty`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List-NonEmpty.html#t:NonEmpty) data type as an example.

`NonEmpty'` represents a list with at least one element. It has two components: a must-have head `a` and a tail `[a]`, which might be empty.

```haskell
data NonEmpty' a = NonEmpty'
  { neHead :: a
  , neTail :: [a]
  }
```

To define the Functor instance, we need to implement `fmap`.

We define it by pattern matching on the head and the tail of the non-empty list. Applying `f` to the head `x` takes care of it, but what about the tail?

\* Note: to be able to write the type signature of `fmap` in an instance definition, you need to use the [`InstanceSigs` extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-InstanceSigs).

```haskell
instance Functor NonEmpty' where
  fmap :: (a -> b) -> NonEmpty' a -> NonEmpty' b
  fmap f (NonEmpty' x xs) = NonEmpty' (f x) :| ??
```

The tail is a regular list, and `fmap` for list is already defined, so we can just use that:

```haskell
instance Functor NonEmpty' where
  fmap :: (a -> b) -> NonEmpty' a -> NonEmpty' b
  fmap f (NonEmpty' x xs) = NonEmpty' (f x) (fmap f xs)
```

However, let’s see how it could be implemented just to understand the mechanics of `fmap` better.

```haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap f x:xs = f x : fmap f xs
  fmap _ []   = []
```

In fact, the principle is the same — splitting a list on head and tail, and calling the function recursively. On each iteration, we apply `f` to the head and call `fmap` with the tail of the list.

In the end, we need to cover the empty-list case — ‘fmapping’ an empty list gives an empty list.

It’s done! You’ve just seen how to define your own instance of the Functor typeclass. You can practice this more with the exercises section.

## Functor is not always a container

There is one noteworthy fact about Functor — it’s not always a container. For example, a function is a Functor too!

But is there any data that corresponds to the function? The answer is yes, it’s `(->)`:

```haskell
> :info (->)
type (->) :: * -> * -> *
data (->) a b
```

`(->)` is parametrized with two types — `(->) a b`. In other words, it is `a -> b` — a function with one argument.

We know that Functor can be implemented for types with the  `* -> *` kind. Unfortunately, the kind of `(->)` is `* -> * -> *`. It doesn’t satisfy the Functor instance in this form because a Functor can only have one type argument.

Hence, it’s required to apply it once, like `Either a` and `((,) a)` do. In the `(->)` case, it means providing the function argument’s type — `(->) a` or `a ->` (the syntax of the latter is not allowed in Haskell, though).

For example, functions with the following type signatures have correct Functor instances:

```haskell
Int -> a
String -> a
(Char, Int) -> a
[Int] -> a
```

We found out what functions could be functors and what data they match. So, the next question — what does it mean to `fmap` the function? Intuitively, it’s about changing the function, i.e., the action it performs. But what is the fixed context here?

Let’s construct the type definition of `fmap` for a one-argument function. For an arbitrary Functor `f`, `fmap` is:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

The type of our data is `(->) c`. Therefore, we need to replace `f t` by `(->) c t` or `c -> t`. Consequently, the type signature is:

```haskell
fmap :: (a -> b) -> (c -> a) -> (c -> b)
```

Now we can see that the fixed context corresponds to the type of function’s argument `c`. And the value being modified is the function’s return type — we go from `a` to `b` with the help of the `a -> b` function.

All in all, `fmap` for functions transforms what the function returns while keeping the input unchanged! Does that look familiar to you?

<details>
<summary>Which Haskell function or operator can produce a <code>c -> b</code> function from <code>a -> b</code> and <code>c -> a</code>?</summary>

Function composition:

```haskell
> :t (.)
(.) :: (a -> b) -> (c -> a) -> c -> b
```

Note that

`(a -> b) -> (c -> a) -> c -> b`

is exactly the same as

`(a -> b) -> (c -> a) -> (c -> b)`

since `->` in Haskell is right-associative.

<hr>
</details>

Cool! `fmap` for Functor is just the composition of functions.

Here’s the `((->) a)` instance implementation:

```haskell
instance Functor ((->) a) where
  fmap = (.)
```

In conclusion, let’s make sure `fmap` is `(.)` for one-argument function `a -> b` with the example below:

```haskell
-- fmap :: (Int -> Int) -> (Int -> Int) -> Int -> Int
> fmap (+1) (*2) 2
5
> (+1) . (*2) $ 2
5
```

In fact, it’s uncommon to consider a function a Functor in Haskell, it’s more of a fancy case. However, you’ll not be puzzled when you come across magic and strange usage of `fmap` in the future!

## Why do you need to know Functor?

In general, Functor is not an extraordinary typeclass. Its methods are easy to grasp and to implement.

Nevertheless, a Haskell project can hardly do without a couple Functor instances. It enables one to apply transformation on the wrapped type without knowing anything about the wrapper, and it's very beneficial.

Moreover, Functor is a solid basis and the predecessor of the **Applicative** typeclass, which further leads to monads. Hence, it brings you one step closer to the dream of many — understanding monads.

## Exercises

1. Implement Functor for the [binary search tree](https://en.wikipedia.org/w/index.php?title=Binary_search_tree&oldid=1053783914).

    ```haskell
    data BinarySearchTree a
      = Branch (BinarySearchTree a) a (BinarySearchTree a)
      | Leaf
    ```

    Expected behaviour:

    ```haskell
    > bst = Branch (Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)) 4 Leaf
    > (+1) <$> bst
    Branch (Branch (Branch Leaf 2 Leaf) 3 (Branch Leaf 4 Leaf)) 5 Leaf
    ```

    <details>
    <summary>Solution</summary>

    ```haskell
    instance Functor BinarySearchTree where
      fmap :: (a -> b) -> BinarySearchTree a -> BinarySearchTree b
      fmap _ Leaf                     = Leaf
      fmap f (Branch left node right) = Branch (fmap f left) (f node) (fmap f right)
    ```

    <hr>
    </details>

2. Implement `(<$)` for `Maybe a`.
    <details>
    <summary>Solution</summary>

    ```haskell
    (<$) :: a -> f b -> f a
    _ <$ Nothing  = Nothing
    x <$ (Just y) = Just x
    ```

    <hr>
    </details>

3. Implement string converter to upper case `toUpperString :: String -> String` without using [toUpper](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html#v:toUpper).

    Expected behaviour:

    ```haskell
    > toUpperString ""
    ""
    > toUpperString "abc"
    "ABC"
    ```

    Assume that the input is correct, i.e. all characters are lowercase English letters.

    <details>
    <summary>Solution</summary>

    ```haskell
    caseDiff = ord 'a' - ord 'A'

    -- applying `fmap`s sequentially
    toUpperString str =
      fmap chr .
      fmap (subtract caseDiff) .
      fmap ord $
      str

    -- applying `fmap` with the composition of functions
    toUpperString str = fmap toUpperChar str
      where
        toUpperChar = chr . subtract caseDiff . ord
    ```

    <hr>
    </details>

4. Prove that the  `instance Functor ((->) a)`  we discussed previously follows the identity and composition laws.

    Identity: `fmap id == id`.

    Composition: `fmap f . fmap g == fmap (f . g)`.

    <details>
    <summary>Solution</summary>

    See the [Functor laws for functions paragraph](https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads/)

    <hr>
    </details>

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
In this article, we'll call such a data type with another type inside it a _context_.

Transforming values inside fixed contexts is common in programming. 

For example, the optional data type ([`Maybe`](https://serokell.io/blog/algebraic-data-types-in-haskell#maybe)) provides a context of a possibly failed computation. It can also be "mapped" by trying to apply a function to the wrapped value without changing the context. 

```haskell
map' :: (a -> b) -> Maybe a -> Maybe b

map' f (Just x) = Just (f x)
map' f Nothing = Nothing
```

```none
> map' (\x -> x + 1) (Just 1)
Just 2
> map' (\x -> x + 1) Nothing
Nothing
```

This article will introduce you to Functor – a typeclass that unifies these kinds of transformations and provides common functionality for them. 

After reading this article, you will know:

- what Functor is in Haskell;
- how to define and use your own Functor instances;
- why and where Functor is useful.

We'll also provide a set of exercises to consolidate your knowledge.

## How to generalize `map`

Look at the type signature of `map`:

```haskell
map :: (a -> b) -> [a] -> [b]
```

It takes a function `a -> b`. Then, it uses that function to change the contents of a list.

Now look at the type of `map'`. It uses the same type of function – `a -> b` – to change the contents of `Maybe`. 

```haskell
map' :: (a -> b) -> Maybe a -> Maybe b
```

The type signatures are kind of similar! 

Could we have a more general function that works for many different contexts? Absolutely. It's Haskell, after all. 

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

Here’s how it works. `fmap` takes an  `a -> b` function and an `f a` data type (`a` wrapped in any context `f`), then the function is applied to what’s inside the context, and finally, a value of type `b` wrapped in `f` is returned. The value can change, but the context remains the same.

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

As you may have guessed, `map` is just a synonym of `fmap` for lists. But it can do much more. With `fmap`, we can do all kinds of actions inside data types like `[]`, `Maybe`, `Either a`, `((,) a)`, and others. 

Of course, we cannot provide an actual implementation of `fmap` that would work for all contexts. Instead, to use it on a data type, that type needs to have an instance of the Functor typeclass. As we'll see, Functor is the thing that provides the said implementation.

## The Functor typeclass

What's the Functor typeclass? Let’s ask GHCi.

```haskell
> :info Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

Functor in Haskell is a [typeclass](https://serokell.io/blog/haskell-typeclasses) that provides two methods – `fmap` and `(<$)`  – for structure-preserving transformations.

To implement a Functor instance for a data type, you need to provide a type-specific implementation of `fmap` – the function we already covered. 

For most common data types, instances of Functor are already implemented. For your own data types, you'll need to define them yourself.

```haskell
data Option a = Some a | None

instance Functor Option where
  fmap f (Some a) = Some (f a)
  fmap f None  = None
```

We'll cover how to do this in more detail later.

### What can be a functor?

There's two things that limit what you can implement a Functor instance for: the kind signature of Functor and its laws. 

#### Kind signature of Functor

The [kind](https://wiki.haskell.org/Kind) of Functor is `(* -> *) -> Constraint`, which means that we can implement Functor for types whose kind is `* -> *`. In other words, we can implement Functor for types that have one unapplied type variable. 

For example, `Maybe` and `[]` take one type variable. Hence their kind is `* -> *`, and there's a straightforward Functor implementation for them. 

```haskell
> :kind Maybe
Maybe :: * -> *
```

`Int` has no type variables, and its kind is `*`, so you cannot create a `Functor` instance for it.

```haskell
> :kind Int
Int :: *
```

`Either` takes two type variables, so it's kind is `* -> * -> *`. But if we apply it once, we can make the kind be `* -> *`. 

```haskell
> :kind Either
Either :: * -> * -> *
> :kind Either String
Either String :: * -> *
```

Therefore, there is a valid Functor instance for an applied `Either` – `instance Functor Either a`, where `a` signifies any variable that could be applied.

#### Functor laws

Some of Haskell typeclasses have laws – conditions that instances have to meet. Usually, these laws come from the math concept of the same name. 

A functor is a [mapping](https://en.wikipedia.org/w/index.php?title=Functor&oldid=1093605134) in category theory. Hence, we need `fmap` to adhere to the following laws.

1. **Identity**

    `fmap id x == id x` – applying `id` function to the wrapped value changes nothing.

    Example:

    `fmap id (Just 1) == id (Just 1) == Just 1`

2. **Composition**

    `fmap f (fmap g x) == fmap (f . g) x` – applying `fmap`s sequentially is the same as applying `fmap` with the composition of functions.

    Example:

    `fmap (+1) (fmap (*2) (Just 1)) == fmap ((+1) . (*2)) (Just 1) == Just 3`


You may wonder why you should follow these laws. The answer is simple – if the instance doesn't meet these conditions, the methods of typeclass won't work as expected. You might run into unpredictable behaviour and confuse people that work with your code as well.

Laws aren't enforced by the compiler, hence you need to ensure their correctness yourself. It may be a bit difficult at the beginning, but after a couple of instances, it will start to feel natural.

There is a tool that can verify whether an instance follows laws – [QuickCheck](https://hackage.haskell.org/package/QuickCheck). It checks properties by random testing. You can provide it a property the code needs to satisfy (a typeclass law, in our case), which is then checked on a large number of randomly generated values. You may look at the "Checking the laws" section of [this article](https://mmhaskell.com/blog/2017/3/13/obey-the-type-laws) as an illustration.

### The `(<$)` operator

`fmap` is all you need to define a `Functor` instance. However, the `(<$)` operator is worth looking at too.

Here's the type signature of `(<$)`:

```haskell
(<$) :: a -> f b -> f a
```

The operator takes a value of type `a`, a value of type `b` packed in a functor `f` – `f b` – and produces a functor `f a`. Basically, the operator packs the first argument’s value in the context of the second argument, throwing away the second value.

Now, a little exercise. Let’s try to guess the definition of `(<$)` by using the provided description and the following examples.

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

Pay close attention to the list examples. `y <$ [x1, x2, x3]` results in `[y, y, y]` not `[y]` since the list has many values inside and not one. Each element of the list is transformed instead of the value being wrapped in `[]`.

So, what do you think is the default definition of `(<$)`?

<details>
<summary> The definition of <code>(<$)</code>.</summary>

Exactly! `(<$)` just runs `fmap` with `const` function.	

```haskell
x <$ f = fmap (const x) f
```

<hr>
</details>

Now that we've seen the basic components of Functor, it’s time to create our own instance of this type class!

## How to implement a Functor instance

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

**Note:** to be able to write the type signature of `fmap` in an instance definition, you need to use the [`InstanceSigs`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-InstanceSigs) extension.
	
```haskell
instance Functor NonEmpty' where
  fmap :: (a -> b) -> NonEmpty' a -> NonEmpty' b
  fmap f (NonEmpty' x xs) = NonEmpty' (f x) ??
```

The tail is a regular list. And `fmap` for lists is already defined, so we can just use that:

```haskell
instance Functor NonEmpty' where
  fmap :: (a -> b) -> NonEmpty' a -> NonEmpty' b
  fmap f (NonEmpty' x xs) = NonEmpty' (f x) (fmap f xs)
```
	
However, let’s explore the implementation to better understand the mechanics of `fmap`.

```haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap f x:xs = f x : fmap f xs
  fmap _ []   = []
```

In fact, it follows the same principle – splitting a list on head and tail and calling the function recursively. On each iteration, we apply `f` to the head and call `fmap` with the tail of the list. In the end, we need to cover the empty-list case – "fmapping" an empty list gives an empty list.

And it’s done! You’ve just seen how to define an instance of Functor. If you wish to practice more, the [exercise section](https://serokell.io/blog/whats-that-typeclass-functor#exercises) has an additional implementation exercise for you to try.

## Functor is not always a container

There is one noteworthy fact about Functor – it’s not always a container. The laws of Functor permit some rather wild implementations. For example,  functions are functors too!

But is there any data type that corresponds to functions? The answer is yes. It’s `(->)`.

```haskell
> :info (->)
type (->) :: * -> * -> *
data (->) a b
```

`(->)` is parametrized with two types – `(->) a b`. In other words, it is `a -> b` – a function with one argument.

We know that Functor can only be implemented for types with the  `* -> *` kind. Unfortunately, the kind of `(->)` is `* -> * -> *`. It doesn’t satisfy the Functor instance in this form because a Functor can only have one type argument.

Hence, it’s required to apply it once, like we do with `Either a` or `((,) a)`. In the `(->)` case, it means providing the function argument’s type – `(->) a` or `a ->` (the latter is not valid syntax in Haskell, though).

For example, functions with the following type signatures have Functor instances:

```haskell
Int -> a
String -> a
(Char, Int) -> a
[Int] -> a
```

So we've found out what functions can be functors and what data they match. The next question is – what does it mean to `fmap` a function? Intuitively, it’s about changing the function, i.e., the action it performs. But what is the fixed context here?

Let’s construct the type definition of `fmap` for a one-argument function. For an arbitrary Functor `f`, `fmap` is:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

The type of our data is `(->) c`. Therefore, we need to replace `f t` by `(->) c t` or `c -> t`. Consequently, the type signature is:

```haskell
fmap :: (a -> b) -> (c -> a) -> (c -> b)
```

Now we can see that the fixed context corresponds to the type of function’s argument `c`. And the value being modified is the function’s return type – we go from `a` to `b` with the help of the `a -> b` function.

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

It’s uncommon to consider a function a Functor in Haskell. In fact, it’s more of a fancy case. However, you’ll not be puzzled when you come across this magical and strange usage of `fmap` in the future!

## Why is Functor useful?

In general, Functor is not an extraordinary typeclass. Its methods are easy to grasp and to implement.

Nevertheless, a Haskell project can hardly do without a couple of Functor instances. It enables one to do transformations on the wrapped type without knowing anything about the wrapper, and that's very beneficial.

Moreover, Functor is a solid basis and the predecessor of the Applicative typeclass, which further leads to Monad. The latter is a famous and widely used typeclass in Haskell. Understanding it will give you considerably greater command of the language.
	
## Conclusion 
	
In this article, we have covered the Functor typeclass: what it is, where it can be used, and how to implement your own instances of Functor. It's a part of larger [What's That Typeclass](https://serokell.io/blog/what's-that-typeclass) series, where we introduce readers to commonly used Haskell typeclasses. 
	
If you want to read more articles from Serokell, be sure to follow us on [Twitter](https://twitter.com/serokell) or subscribe to the newsletter via the form below.
	
And finally, if you see anything that's wrong with the article or if there's something you don't understand, you're welcome to submit an issue in our [GitHub repo](https://github.com/serokell/blog-posts) – we'd appreciate that greatly! 

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

3. Implement a function that converts strings to upper case (`toUpperString :: String -> String`) without using [toUpper](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html#v:toUpper).

    Expected behaviour:

    ```haskell
    > toUpperString ""
    ""
    > toUpperString "abc"
    "ABC"
    ```

    Assume that the input is correct, i.e., all characters are lowercase English letters.

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

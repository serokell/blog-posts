## Haskell and Dimensions: Singletons in Action


#### Intro


[In our previous blogpost](https://serokell.io/blog/dimensions-and-haskell-introduction), we introduced a reader to our subject matter
and briefly observed several numeric libraries in Haskell. We explained why we don’t use one of the popular libraries called [`GHC.TypeLits`](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html) with non-inductively defined kind of type-level natural numbers.
In our approach, we put dimensions of data types on the type level to avoid a large set of errors that might arise at the runtime stage.
During the compile stage, one may check some properties of matrix dimensions expressed via type-level natural numbers and find out the reason of that error as a type error.
In this part, we describe our approach to the matrix data type that parameterised via its numbers of columns and rows.


#### Foreword


We had a task to implement a few machine learning algorithms for [dimensionality reduction](https://en.wikipedia.org/wiki/Dimensionality_reduction). One important goal was to find out whether Haskell fits for such sort of tasks and make an open-source library for dimensionality reduction.
At first, we took a look at the [Accelerate](http://hackage.haskell.org/package/accelerate) library due to its efficiency in parallel computations, especially using GPU. We found out that a few required functions from linear algebra were not implemented in it. By the way, we are going to implement these functions later via bindings to CUDA-solver. However, we decided to switch to [REPA](http://hackage.haskell.org/package/repa) as a base matrix library because it contains the desired functionality.
During the implementation of the given algorithms we encountered "debugging hell", a condition, when it wasn't possible to define the place of a mistake even if you had the place of an error. (Especially in case of PPCA which can handle and restore missed data in the input.) That's what we had:

```
GPLVMHaskell-exe: inconsistent dimensions in matrix product (11,2) x (3,1)
CallStack (from HasCallStack):
error, called at src/Internal/LAPACK.hs:61:31 in
hmatrix-0.19.0.0-GJ4OJPujscCE7zmZ8JSwjL:Internal.LAPACK
```
Where is the place of the exception? Why such specific dimensions are there? It is a real detective job, honestly.  Each time we dealt with such errors, we launched an inquiry. It was quite monotonous and exhausting. We wanted to make debugging easier and decided to lift dimensions on the type level. 

Why type-level dimensions?

1. A lot of errors affect interim array dimensions. 
2. We have information on unsatisfied general conditions rather than some specific numbers of dimensions that depend on the input data in the case of type errors. 
3. A chance of runtime error appearance is very low on every acceptable input data – at least of error which influenсes the dimensions.

So, we decided to test this approach.


#### The Definition of Matrix


As we have said before, one needs to check matrix dimensions and their properties at the type level. For this purpose, we promoted dimensionality into the type of matrix.


`DimMatrix` is a newtype on `Matrix r a`. Note that dimensional type parameters are only on the left side of this definition.
```haskell
newtype DimMatrix r (y :: Nat) (x :: Nat) a = DimMatrix { getInternal :: Matrix r a }
```
Here `a` is a type of elements, `r` is a representation type. `y` and `x` are types of kind `Nat` from `type-natural` library, which is the most useful for our goals, as we discussed in the introduction.

Looks good, but the dimensionality of the input data is unknown at compile-time. Thus, types might be dependent on other values received at the runtime stage. This connection might be described quite straightforwardly via dependent types. Here's a small example written in Agda:

```agda
generateVec : {A : Set} → (ℕ → A) → (n : ℕ) → Vec A n
generateVec f zero = []
generateVec f (suc n) = (f $ suc n) ∷ generateVec f n
```
In this example, we generate a list with length in its type, the result of which is parametrised by value `n`. In other words, it depends on the value. Here, the benefit of dependent types is that the compiler checks the function body. If the length of that list doesn't equal `n` or the compiler cannot prove this fact, then we obtain a compilation error.
At present, Haskell lacks dependent types. However, there is the necessity to jump from values to types, and we're not able to do it with the actual Haskell type system.

In [`singletons`](http://hackage.haskell.org/package/singletons), one may emulate a dependent type by jump mapping some type `a` into a datatype `Sing n`, which has exactly one value in runtime. Read [this article](https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf) to learn more about this tool.
The basic module called `Singletons.Prelude` provides singleton types, prelude-like type-level functions, and promoted types. The main goal of the module is to emulate Haskell prelude at the type-level. More information about promoted types you can find [here](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html).
This library is pretty helpful for dependent types emulating in Haskell, but it might become irrelevant when [full-fledged dependent](https://serokell.io/blog/2018/12/17/why-dependent-haskell) types would be available in Haskell. [This tutorial](https://blog.jle.im/entry/introduction-to-singletons-1.html) introduces singletons more systematically. We only discuss some basic constructions that we have used for type-safe dimensions.

We decided to use the `singletons` interface for type-level dimensions. Here we meet an additional important characteristic of `type-natural`. There is integration between `type-natural` and `singletons` implemented [as follows](https://hackage.haskell.org/package/type-natural-0.8.2.0/docs/Data-Type-Natural.html#g:3).

Let us consider the following example of `singletons` use with `DimMatrix` data type. `withMat` is a function that creates the same matrix with type-level dimensions from the input `repa` matrix. We implemented this function via continuation-passing style because type-level dimensions `x` and `y` are bound by the internal universal quantifier so that they cannot appear in the result type `k`. Here, we use a continuation-passing style to create a matrix with type-level dimensionality from the usual one and avoid the disappearance of dimension in types.
This function is one of the most widely used by us:
```haskell
withMat
  :: Matrix D Double
  -> (forall (x :: Nat) (y :: Nat). (SingI y, SingI x) => DimMatrix D x y Double -> k)
  -> k
withMat m f =
    let (Z  :.  y  :.  x) = extent m
    in
    case toSing (intToNat y) of
      SomeSing (sy :: Sing m) -> withSingI sy $
        case toSing (intToNat x) of
          SomeSing (sx :: Sing n) -> withSingI sx $ f (DimMatrix @D @m @n m)
```

#### Property Checking


At the early stages, we created proofs via the function called [`unsafeCoerce`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Unsafe-Coerce.html#v:unsafeCoerce), if some desired condition holds. After that, we used the [`Evidence`](http://hackage.haskell.org/package/dimensions-1.0.1.1/docs/Numeric-Type-Evidence.html#t:Evidence) data type that came from `dimensions`, where, however, type-level proofs are created via the same `unsafeCoerce`. Here is a simple example from the library:

```haskell
sameDims :: Dims (as :: [Nat]) -> Dims (bs :: [Nat]) -> Maybe (Evidence (as ~ bs))
sameDims as bs
  | listDims as == listDims bs
    = Just (unsafeCoerce# (E @('[] ~ '[])))
  | otherwise = Nothing
```

In addition to type-safe dimensions itself, we also need to check their properties. For example, we need to make sure that the number of columns is less or equal to the input number. We have already promoted our dimensionality into the type level, but we also should verify their properties at the type level. Let's look at a simple example.

```haskell
...

  case (sing :: Sing desired) %<= (sing :: Sing x) of
    Proved LEQ -> foo @desired @x
    Disproved _ -> error "Something went wrong"
...

foo :: forall (d :: Nat) (x :: Nat). (d <= x ~ 'True) => ...
```
where `desired` and  `x` are types of the kind `Nat`, unknown at compile-time; `foo` is an arbitrary function. Here, `LEQ` is a constructor of the data type `(:<=:)` that we introduce below.
This constructor stores a proof that the first argument is less or equal to the second one.
Here, `@` came from the language extension called [`TypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications). This extension allows us to apply functions to types as arguments explicitly.

The example above is quite close to real code. We don’t know any specific dimensions at compile-time; validation of the property occurs at runtime. We ensure type system that the property holds and in the case of success, we can use it and satisfy the constraint of the function called `foo`. After that, we use this property in `foo` and other functions which will be called in `foo`.

`(:<=:)` is a data type implemented as GADT that keeps a justification that $a \leq b$:

```haskell
data (a :: k) :<=: (b :: k) where
  LEQ :: forall k (a :: k) (b :: k). ((a <= b) ~ 'True) => a :<=: b
```
In other words, it can’t be even constructed if `a > b`. And the very fact of its existence proves the property. It is similar to the method of the type class [`SDecide`](https://hackage.haskell.org/package/singletons-2.5.1/docs/Data-Singletons-Decide.html#t:SDecide) called `%~` type from `singletons` which creates a proof of [propositional type equality](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html#t::-126-:). Similarly, `(%<=)` is a method of the kind class `LEQDecide` that we've introduced:


```haskell
class LEQDecide k where
  (%<=) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :<=: b)
  infix 4 %<=
```
It compares two values of the `Sing x` type at runtime and yields a value of the type [`Decision`](http://hackage.haskell.org/package/singletons-2.5.1/docs/Data-Singletons-Decide.html#t:Decision) `(a :<=: b)`. Let us describe the `Decision` data type. In some cases, one may prove decidability of certain relations and predicates. In logic, a proposition $P$ is decidable if either $P$ is provable or $\not P$. In other words, we have a way to tell “Yes” or “No” based on what exactly is provable: the statement or its negation. In Haskell, the `Decision` data type expresses the decidability of the proposition. This type consists of the following two constructors. The first one is called `Proved`. This constructor stores the term that proves the desired statement. The other constructor `Disproved` contains a proof of negation, that is, a function `a -> Void`, so far as an empty type is an absurd constant logically.


#### Type-safe Matrix Operations


Let's look at some examples of matrix operations that we implemented via our dimensional matrix data type. In these examples, functions `mulM` and `transposeM` are exactly sequential matrix product and transpose functions from [`repa-linear-functions`](http://hackage.haskell.org/package/repa-linear-algebra) carried through our `DimMatrix` data type. It’s not so difficult, really:
```haskell
mulM
  :: forall y1 x1 y2 x2 r. (x1 ~ y2)
  => DimMatrix r y1 x1 Double -> DimMatrix r y2 x2 Double -> DimMatrix r y1 x2 Double
mulM (DimMatrix m1) (DimMatrix m2) = DimMatrix $ m1 `mulS` m2


transposeM :: DimMatrix r y x Double -> DimMatrix r x y Double
transposeM (DimMatrix m) = DimMatrix $ transpose m
```


##### Principal Component Analysis


PPCA is one of the simplest procedures of dimensionality reduction. PCA is a widely used technique in pattern recognition and data compression. The process of principal components computation reduces to the finding [eigenvectors and eigenvalues](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors) of the given covariance matrix. A covariance matrix is a degree of a spread of data in the given observation set. An eigenvector is a non-zero vector such that an application of the given linear operator yields the same vector up to a scalar factor, i. e. eigenvalue. You may read the more detailed description of PCA and its extensions [here](ftp://statgen.ncsu.edu/pub/thorne/molevoclass/AtchleyOct19.pdf). Also, there is quite informative [visualisation](http://setosa.io/ev/principal-component-analysis/) of this procedure.

PCA works as follows. Suppose we have some data set, which defines as a two-dimensional matrix $M \in \mathbb{R}^{n \times m}$ of real numbers and each row represents a single observation. At the next step, we need to subtract the mean from our data set for each dimension to obtain a new equivalent data set, which mean equals to zero. After that, we compute the covariance matrix, eigenvectors and eigenvalues to form the set of feature vectors. There is a statement that eigenvectors of covariance matrix form a new basis in the observed space. Eigenvector with the largest eigenvalue forms the axe with the highest dispersion along with it and the lower eigenvalue, the lower dispersions along with the corresponding axe. We can drop eigenvectors with the lowest eigenvalues and reduce the dimension with fewer information losses.
Finally, the reduced principal component matrix is the product of a feature vector matrix and a transposed mean-adjusted data set that we have already obtained on the previous step. Note that the number of intended principal components is passed as a separate parameter.


Now, we take a look at our PCA implementation in Haskell. We define PCA as a record data type as follows:


```haskell
data PCA = PCA
  { _inputData      :: Matrix D Double
  , _covariance     :: Matrix D Double
  , _eigenVectors   :: Matrix D Double
  , _eigenValues    :: Matrix D Double
  , _finalData      :: Matrix D Double
  , _restoredData   :: Matrix D Double
  , _meanMatrix     :: Matrix D Double
  }
```
Names of these fields correspond to their roles: `_inputData` is an input matrix, etc. The type-safe version of `PCA` data type is implemented via `DimMatrix` data type, which we have already introduced above.


```haskell
data TypeSafePCA =
  forall x y d. (d <= x ~ 'True) => TypeSafePCA
  { desiredDim    :: Proxy d
  , inputData_    :: DimMatrix D y x Double
  , covariance_   :: DimMatrix D x x Double
  , eigenVectors_ :: DimMatrix D x x Double
  , eigenValues_  :: DimMatrix D x One Double
  , finalData_    :: DimMatrix D y d Double
  , restoredData_ :: DimMatrix D y x Double
  , meanMatrix_   :: DimMatrix D y x Double
  }
```
In this data type, we have [existentially quantified](https://wiki.haskell.org/Existential_type) dimensions, where `y` is a number of columns, `x` is a number of rows, `d` is a required number of rows for the final data in an output matrix. Also, we pass a justification that $d \leq x$ as the coercion constraint `d <= x ~ 'True` between type-level less or equal predicate and Boolean value `True` promoted via [`DataKinds`](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html).

Now we have the following set of function that makes PCA from an input matrix:

```haskell
makePCA :: Int -> Matrix D Double -> PCA
makePCA dim input =
  case toSing (intToNat dim) of
    SomeSing (sd :: Sing desired) -> withSingI sd $ withMat input $ \(inputMatrix :: DimMatrix D y x Double) ->
      case checkInput (Proxy @desired) (Proxy @x) of
       Proved LEQ -> convertTypSafeToPCA $ makePCATypeSafe (Proxy @desired) inputMatrix
       Disproved _ -> error "Error: desired dimension is greater than an old number of rows"
```
The `makePCA` function takes a new number of dimensions and matrix of real numbers as arguments and yields `PCA` record. In this function, we promote our required dimension using the functions called `toSing` and `intToNat`, where `toSing` is a method `SingKind` kind class. `intToNat` is a map between integers and natural numbers defined quite naturally. The result of this embedding is a value of type `SomeSing (Sing desired)`, where `desired` is our integer argument obtained after this sophisticated promotion and `SomeSing` is a container for a singleton unknown at compile-time.


`checkInput` is a function that yields the decision of $d \leq x$, where `d` and `x` are proxy arguments. Note that these type-level naturals should have instances of the `SingI` type class. It ensures that our type has a corresponding singleton type.


```haskell
checkInput
  :: forall (d :: Nat) (x :: Nat). (SingI d, SingI x)
  => Proxy d -> Proxy x -> Decision (d :<=: x)
```

The main logic is implemented in the function called `makePCATypeSafe` according to the informal description above.


```haskell
makePCATypeSafe
    :: forall d x y. (AllConstrained SingI '[d,x,y], d <= x ~ 'True)
    => Proxy (d :: Nat) -> DimMatrix D y x Double -> TypeSafePCA
```
where `AllConstrained` is a type family from [`vinyl`](http://hackage.haskell.org/package/vinyl-0.11.0/docs/Data-Vinyl-TypeLevel.html#t:AllConstrained) that applies the given constraint to the type-level list.


##### Probabilistic Principal Component Analysis


In contrast to PCA, which is completely linear algebraic, [probabilistic principal component analysis](http://www.robots.ox.ac.uk/~cvrg/hilary2006/ppca.pdf), or PPCA, is a probabilistic version of PCA. PPCA is a probabilistic extension of PCA. This technique defines principal axes of a matrix via maximum-likelihood estimation applying well-known [expectation-maximization algorithm](http://cs229.stanford.edu/notes/cs229-notes8.pdf) (or, EM-algorithm). We have two versions of PPCA. The first one is PPCA with so-called missed data. The second one lacks it.

Informally, PPCA works as follows. Let $\{ x_i \}_{i \in \{ 1, \dots, m\}}$ be a data set, where $x_i \in \mathbb{R}^n$ and one needs to find a way to represent these data points as $\{ z_i \}_{i \in \{ 1, \dots, m\}}$, where $z_i \in \mathbb{R}^{d}$ and $d < n$. The statement of the problem tells us that we need to optimise our data set somehow. That's the same dimensionality reduction task, but, as you know, the devil is in the detail. In the case of PPCA, we work with the following linear model:

$z = W x + \mu + \varepsilon$

where $W \in \mathbb{R}^{d \times n}$ is a [linear transformation matrix](http://mathworld.wolfram.com/LinearTransformation.html); $\varepsilon$ is [Gaussian noise](https://www.quora.com/What-is-Gaussian-noise); $\mu$ is a mean. One should reach the estimation of the linear transformation matrix $W$ [maximal likelihood](https://en.wikipedia.org/wiki/Likelihood_function). There is a way to obtain this estimation straightforwardly, but it’s very inefficiently.

And here comes the EM-algorithm. It is an iterative algorithm that consists of the following steps:
1) The initialisation of $\{\bf W}$ and $\sigma^2$ by random values, where $\{\bf W}$ is a linear transformation matrix from the definition above and $\sigma^2$ is a variance (in other words, squared expectation).
2) After that, we obtain the set of latent variables ${\bf Z} = {\bf z}_n$ from the corresponding a posteriori distribution of conditional probability $p({\bf z}| {\bf x})$.
3) Let us assume that ${\bf z}_n$ is fixed. After that, we seek the values of the linear transformation map $\{\bf W}$ and variance $\sigma^2$. These parameters provide a maximal expectation of logarithm likelihood $E[ln p({\bf X}, {\bf Z} | \mu, \{\bf W}, \sigma^2)]$.
4) If those changes in parameters are greater than the initial value, then we return to step 2 and seek a new $\{\bf Z}$, $\{\bf W}$ and $\sigma^2$. Otherwise, we are done.


This way has a few advantages:
1. It is much faster than the exact solution.
2. The solution with the EM-algorithm can handle and restore missed values in our observations with a little modification.
3. We can use the estimation of logarithm likelihood and variance to assess the quality of the result. Read paragraph 12.2 in “[Pattern recognition and machine learning]” (https://www.springer.com/gp/book/9780387310732) to understand PPCA and EM algorithms better.

Let us consider how we formalised this procedure in Haskell. We introduce `PPCA` record data type with input and output fields with the Boolean flag on the presence of missed data.


```haskell
data PPCA = PPCA
  {  _noMissedData       :: Bool
   , _learningData       :: Matrix D Double
   , desiredDimensions   :: Int
   , stopParameter       :: Either Int Double
   , _variance           :: Double
   , _W                  :: Matrix D Double
   , _finalExpLikelihood :: Double
   , _restoredMatrix     :: Maybe (Matrix D Double)
  }
```
Input parameters:
`_noMissedData`       - If there are no values marked as `NaN`, then, in the case of `True`,
                        we run the fast version of the EM algorithm, which doesn’t try to
                        restore missed values.
`_learningData`       - The set of observations, matrix $M \times N$
`desiredDimensions`   - The desired dimension of latent space. This number should be less or equal to $M$
`stopParameter`       - This field stores either the number of iterations or maximally allowed
                        change of elements of ${\bf W}$ matrix between iterations.
Output parameters:
`_variance`              - The final value of $\sigma^2$
`_W`                     - The transformation matrix between latent space and observed space
`_finalExpLikelihood`    - Expectation of logarithm likelihood
`_restoredMatrix`        - The matrix with restored values. If there are no missed values in
                           `_learningData`, then it will be `Nothing`.


The function `makePPCATypeSafe` takes observations, the required dimension of latent space, and termination condition. This function generates random values for ${\bf W}$ and $\sigma^2$. This function also creates matrices with dimensions in their types and runs either `emStepsFast` or `emStepsMissed`. Finally, the function transforms type-safe matrices of the result into the usual matrix type and yields `PPCA` record.


```haskell
makePPCATypeSafe :: RandomGen gen => Matrix D Double -> Int
  -> Either Int Double -> gen -> PPCA
```
The `emStepsFast` function takes observations, initial values of the linear transformation matrix ${\bf W}$ and variance $\sigma^2$, and the termination condition. The result is final ${\bf W}$, $\sigma^2$ and expectation of likelihood logarithm. Note that we require some properties of dimensions in the constraint. The function `emStepsMissed` of the same type is also quite fascinating:


```haskell
emStepsFast, emStepsMissed :: forall d y1 x1 y2 x2.
  (x2 ~ d , y1 ~ y2, (One <= x1) ~ 'True, AllConstrained SingI [y2, x1, d])
  => DimMatrix D y1 x1 Double -> DimMatrix D y2 x2 Double
  -> Double -> Either Int Double
  -> (DimMatrix D y2 x2 Double, Double, Double, Maybe (DimMatrix D y1 x1 Double))
```
`emStepsMissed` also returns the matrix of observations with restored values. Let's look at the function more closely. It is too huge to show the whole function, so we consider the implementation partially. First of all, let us notice that there are local functions that return matrices which dimensions depend on the elements. For instance:
```haskell
...
  oldWP :: forall i toDel. ((i <= x1) ~ 'True, (toDel <= y1) ~ 'True, SingI i)
           => Proxy i -> Proxy toDel -> DimMatrix D (y1 - toDel) x2 Double
  oldWP iP _ = withListOfIndexes @y1 (unknownIndexes (LessEq iP)) (deleteRowsM @toDel oldW)
...
```

We use this function to create the set of `x1` matrices ${\bf OldW}_{present}$. We remove the rows from ${\bf OldW}$ with the same index as the index of unknown value in the $i$-th column of observations matrix for each $i \in \{0, \dots, x_1}\$ . Here ${\bf OldW}$ is ${\bf W}$(transformation matrix between spaces) from the previous iteration. As a result, we have the matrix with `(y1 - toDel)` columns, where `toDel` depends on the number of unknown values in the $i$-th column of unknown values. Its value is unknown at compile-time, but we can ensure the type checker that we checked its property (`(toDel <= y1) ~ 'True`) using singletons in the same way as we have described before.
`LessEq` is a constructor of data type `LEQThan (x :: Nat)` and consists of `Proxy i`. One may create a value of this type only if $i \leq x$.


Secondly, we may find this quite a strange piece of code.

```haskell
expX_ ::forall (i :: Nat). ((i <= x1) ~ 'True ) => Sing i  -> DimMatrix D x2 i Double
expX_ SZ = emptyM :: DimMatrix D x2 Zero Double
expX_ (SS l) = case lemma1 l (Sing :: Sing x1) of LEQ -> withSingI l $ (expX_ l) ^++^ ((withDelNumber expXi) (LessEq (Proxy :: Proxy (i - One))))
```
Here we form the expectation (in terms of probability theory) of missed values and other elements in the presence of such missed values. Of course, we can't restore the real values of missed cells, this algorithm just finds an expectation of all values.  `expX_` is a recursive function: at every step of recursion, the function adds a new column to the accumulator. It is a suitable example of work with dependent types. The compiler checks the body of this function and ensures that this function creates a matrix with exactly `i` columns at runtime stage. 

On the other hand, there is also `lemma1`. Why do we need it? Unfortunately, the type checker is not so smart as we are. We should prove such trivial statements as this one:
```haskell
lemma1 :: forall (n :: Nat) (x :: Nat). (('S n <= x) ~ 'True) => Sing n -> Sing x -> (n :<=: x)
lemma1 SZ _ = LEQ
lemma1 (SS l) (SS k) = case lemma1 l k of LEQ -> LEQ
```
It is obvious for us that $(n + 1) \leq x$ implies $n \leq x$, but not for the compiler. Of course, we may merely apply `unsafeCoerce` for similar examples, but we prefer to use it as rarely as possible. Unsafe coercion is not the way for more complicated examples.
We need this proof because at each iteration except the last one, we call this function again on `(i -1)`, but we proved only that `i <= x1`, not `(i - 1) <= x1`.

We didn't use the `singletons` functionality initially since dimensions of intermediate matrices don't depend on their values. In other words, these dimensions depend on the input values and applied operations at each step. The case of PPCA with missed data is completely different in this aspect. That's why we had to use `singletons` when we were working on this algorithm. A need to infer some form of properties of such dimensions that may depend on intermediate matrices values caused `singletons` use. By the way, one can verify within the type system quite safely that required property really has a proof as we have already shown above.

#### Summary


We discussed one way to reduce the debugging time and make our programs less error-prone. In this approach, matrix dimensions are lifted to the type level with the use of the Singletons library. At first glance, our solution looks a bit sophisticated. Why? There remains a question about the way to make it less devious and more idiomatic. In other words, we have to recognise the restrictions of Haskell expressive opportunities. What about performance? Also, our approach helps to remove only errors that affect the dimensions of arrays. Can we track other array parameters to reduce a set of possible runtime errors even more? We’ll talk about it in the next part of our article.

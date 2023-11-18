## Preface

At first, I was modelling a functional language with `TypeApplication`, `RankNTypes` and `Impredicativity`. But at some point I scrapped and recycled the idea in favour of a language with dependent types. The reason was simple - to prevent duplication of code and reduce the count of namespaces to handle. This, however, had consequences, but more on that later.


## Language

Okay, lets describe the language we model.

I will use the following convention:

- `A`, `B`, `C`  - programs;
- `f`, `g` - field names;
- `T`, `U` - types;
- `K` - kinds;
- `x`, `y` - variables.

The programs, types and kinds in our language are the same entity, but for the sake of clarity in certain places it is useful to think of them as "types" and "kinds".

We have:

<table>
  <tr>
    <th>
      Syntax
    </th>
    <th>
      Explanation
    </th>
  </tr>
  <tr>
    <td>
      <pre>
        x, y, z
      </pre>
    </td>
    <td>
      variables
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        A B
      </pre>
    </td>
    <td>
      application (function calls)
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        fun (x : T) -> B
      </pre>
    </td>
    <td>
      lambdas (function literals)
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        pi (x : T) -> B x
      </pre>
    </td>
    <td>
      dependent function types (result type can depend on parameter)
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        Type
      </pre>
    </td>
    <td>
      the type of all types (including the `Type` itself)
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        case list1 of
          | Empty     -> 0
          | Push x xs -> plus x (sum xs)
      </pre>
    </td>
    <td>
      pattern matching
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        {f = 1, g = "a"}
      </pre>
    </td>
    <td>
      record expressions
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        #{f : Integer, g : String}
      </pre>
    </td>
    <td>
      record types
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        foo.bar
      </pre>
    </td>
    <td>
      record read access
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        let x : T = B in
        let
          data List (a : Type) where
            | Empty : List a
            | Push  : a -> List a -> List a
        in
          ...
      </pre>
    </td>
    <td>
      declarations (including new type ones)
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        let rec
          x : T = B;
          y : U = C;
          ...
        in
          ...
      </pre>
    </td>
    <td>
      mutually recursive declarations
    </td>
  </tr>
  <tr>
    <td>
      <pre>
        _
      </pre>
    </td>
    <td>
      a hole, request for compiler to infer or "I don't care what type it is" statement
    </td>
  </tr>
</table>

## Substitution

The first thing we need to do is to deal with names. It is generally useful to have them in the language, but it introduces problems - mainly, shadowing.

There are lot of ways to deal with it:

<table>
  <tr>
    <th>
      Solution
    </th>
    <th>
      PROs
    </th>
    <th>
      CONs
    </th>
  </tr>
  <tr>
    <td>
      Naive substitution
    </td>
    <td>
      <ul>
        <li> Easy to implement - descent and rename vars, stop when finding that name is re-defined.</li>
        <li> The code can be pretty printed easily.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li> It is slow to be used in production. N strings comparisons OR map lookups when calling or unifying a function, where N is a count of variables in it.</li>
      </ul>
    </td>
  <tr>
  <tr>
    <td>
      De Brujin indexing/locally nameless/<a href=https://hackage.haskell.org/package/bound>Bound</a>
    </td>
    <td>
      <ul>
        <li> You can check if two programs are the same by just comparing them structurally.</li>
        <li> The context is just a stack.</li>
        <li> There is a library that does the thing.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li> <a href="https://www.slideshare.net/ekmett/bound-making-de-bruijn-succ-less">You'll have to walk the tree a lot</a>, tweaking each variable positions.</li>
        <li> It is a good <a href=https://twitter.com/edwinbrady/status/857585751684501504>cylon detector</a>, because making it right from the first time might be quite hard for a mere human.</li>
        <li> You have to store names at binders, if you want to print something the humans can wrap their heads around.</li>
      </ul>
    </td>
  <tr>
  <tr>
    <td>
      Free monads + uniquifyng all the names
    </td>
    <td>
      <ul>
        <li> The substituion is a function from a library (in haskell, at least), the traversal is automatic.</li>
      </ul>
    </td>
    <td>
      <ul>
        <li> Quadratically slow, unless heavy artillery like <a href="https://hackage.haskell.org/package/kan-extensions/docs/Control-Monad-Codensity.html"/>Codensity</a> is used.</li>
      </ul>
    </td>
  <tr>
</table>

I will describe the third approach, because it was easiest to construct.

The idea is simple: you make AST to be a `Functor`with recursion points as holes, and then wrap it with `Free`.

```haskell

data AST_ self
  = Lam_ { arg  :: Name, body :: self }
  | App_ { f    :: self, x    :: self }
  | Let_ { name :: Name, decl :: self, ctx :: self }

data Name = Name { rawName :: Text, uniqueID :: Int }

data Var
  = FreeVar Name
  | Bound   Name

type AST = Free AST_ Name
```

The `Free` makes a tree out of your structure, with subtrees in `self`-points, which in turn can be either `Var`-s or trees again. And that's why there is no constructor for `Var`-s in `AST_`. The `Free` handles that for you.

This doesn't allow you to perform correct substitutions _yet_, but we can describe the substitution machinery first.

We will use maps for substitution:
```haskell
newtype Subst f = Subst { getSubst :: Map.Map Var (Free f Var) }

class Functor f => Substitutable f where
  subst :: Subst f -> Free f Var -> Free f Var
```

You also need a mechanism of name refreshing (which will assign some incremented counter to the given name's `uniqueID`) and a scope.

Assuming we have

```haskell
type Refeshes m :: Constraint

refresh :: Refreshes m => Name -> m Name
```

... the algorithm itself is:

```haskell
instance Functor f => Substitutable (Free f Var) where
  subst (Subst s) prog = do
    var <- prog
    fromMaybe (Pure var) $ Map.lookup var s

instantiate
  :: Substitutable f
  => Name        -- name
  -> Free f Var  -- arg
  -> Free f Var  -- an expression to substitute in
  -> Free f Var
instantiate name arg = subst $ Bound name `Map.singleton` arg

capture :: Refreshes m => Name -> m (Subst f)
capture name = do
  name' <- refresh name
  return $ FreeVar name `Map.singleton` Bound name'
```

Notice, that iе doesn't mention the structure of `AST_` anywhere, which means we have succesfully abstracted it out.

Another thing that will help us is an occurence checker.
```haskell
freeVars :: Free f Var -> Set Name
freeVars = foldMap \case
  FreeVar n -> Set.singleton n
  _         -> mempty

occurs :: Name -> Free f Var -> Bool
occurs n p = n `Set.member` freeVars p
```

I also recommend writing pattern synonyms for each constructor, so you can mostly forget that `Free` even was there.
```haskell
pattern Lam arg  body     = Free (Lam_ arg  body)
pattern Let name decl ctx = Free (Lam_ name decl ctx)
```

_For merging `Subst`-s_, apply one of them to all values in other one first.

## Scopes

We still can't construct programs correctly, let clean this problem up.

I suggest using smart constructor to build `AST`, they also perform capture when needed:

```haskell
lam :: Refreshes m => Name -> AST -> AST -> m AST
lam name ty body = do
  s <- capture name
  return $ Lam (substName s name) ty (subst s body)

let_ :: Refreshes m => Name -> AST -> AST -> AST -> m AST
let_ name ty decl body = do
  s <- capture name
  return $ Let (Val (substName s name) ty decl) (subst s body)
```
We capture the name at binder position as well, because our renaming strategy is not structural, unlike the one used by de Brujin indexing. We have to do it, because otherwise there's no way to tell which of the `a#172` is the `a` defined there. And wil _will_ get big indices.

With this approach, we can carefully control which part of the program is closed over which set of names. For instance, I've left `let` statement non-recursive in this example, and in both smart constructors type cannot depend on the argument.

The interesting part is capturing variables in case alternatives.
```haskell
case ... of
  | pat -> expr  -- those ones
```
My solution was to treat them like lambdas - by substituting both pattern and expression with variables captured in the pattern.

Using that, we can return scope-correct programs right off the parser.

_Pro tip learned after a debug session_: Refreshing names of records field declarations in record literal is a bad idea.

## Unification

We can construct the program. Before we're doing inference, we need an auxillary mechanism. It whill compare two types and either find a set of substitutions to convert both to the same type, or bail out with error if that isn't possible. This mechanism is called unification.

Usially, in Hindley-Milner inference, the type of unifier is

```haskell
unify :: Type -> Type -> Either UnificationError Subst
```

... and the substitution produced on success is applied to all related types that caller uses later. This makes it prone to error, because you could forget to apply some substitution `s7` and this will lead to rather hard time debugging the inference engine.

I'd suggest hiding the accumulated substitutions as `State` inside some monad, and split the unifier onto the part that does the job and the part that applies bindings and adds the resulting ones to the state. Since all the names are unique, the substitutions can never become obsolete.

Please keep in mind that since we have _dependent_ types, the types can and will contain expressions interleaved with types. Also, any value that got to the unifier will be normalized, which means some cases should not be possible.

Lets take apart working half of the unifier:
```haskell
unify
  :: (Refreshes m, HasSubst m, CanThrow UnificationError m)
  => AST
  -> AST
  -> m ()
```
We also need to throw errors, sometimes.

The first part is unification of two free variables:
```haskell
unify (Var n) (Var m)
  | n == m    = mempty
  | otherwise = modify (FreeVar n ==> (Var m) <>)
```
Which is just replacing one with the another.

The next case to check is if only one side is a free variable:
```haskell
unify (Var n) m
  | occurs n m = die $ Occurs n m
  | otherwise  = modify (FreeVar n ==> m <>)

unify m (Var n)
  | occurs n m = die $ Occurs n m
  | otherwise  = modify (FreeVar n ==> m <>)
```
We use the occurence checker from [#Substitution](#Substitution) to check if the resulting type does not refer to itself. How to combine dependent and recursive types is pretty open question.

A bound variable can only unify with itself. It represents some value or type that is not known yet - or can vary - in this region of the program.
```haskell
unify (Bound t) (Bound u) | t == u = mempty
```

For some structures unification is just a descent.
```haskell
unify (App a b) (App c d) = do
  unify a c
  unify b d

unify Star Star = do
  return ()

unify (Lit l) (Lit k)
  | l == k = mempty
```

For functions and their types though, it is a bit more complicated process. We need to unify the type of the argument and then replace the argument in the body of function (or dependent function type) with a name from its counterpart and only then we can unify it.
```haskell
unify (Pi n t b) (Pi m u c) = do
  unify t u
  unify b (subst (Bound m ==> Rigid n) c)

unify (Lam n t b) (Lam m u c) = do
  unify t u
  unify b (subst (Bound m ==> Rigid n) c)
```

Another intesting thing is record and its type. For record, we unify given signatures of two records, and then their field values. For types of records we only unify types.
```haskell
unify (Record ds) (Record gs) = do
  case zipDecls ds gs of
    Just quads -> for_ quads \(tn, tm, n, m) -> do
      unify tn tm  -- unify inferrent types of fields
      unify  n  m  -- unify the fields themselves

    Nothing -> do
      die $ Mismatch (Record ds) (Record gs)

unify (Product tds) (Product tgs) = do
  case zipTDecls tds tgs of
    Just pairs -> for_ pairs \(n, m) -> do
      unify n m

    Nothing -> do
      die $ Mismatch (Product tds) (Product tgs)
```

Sadly, we can do nothing with _projections_. It is possible to have a type expression `a.b`, where `a` is a bound variable and `b` is a field name. It is not reasonable to unify `a ~ b` for `a.x ~ b.x`, because it is possible to have latter without the former. We will consider this an error. Technically, there are ways around it - by handling special cases - but that's not a general solution.
```haskell
unify a@Match {} b = die $ ProjectionL a b
unify a b@Match {} = die $ ProjectionR a b

unify a@Get {} b = die $ ProjectionL a b
unify a a@Get {} = die $ ProjectionR a b
```

The `let`-expression of both kinds should not enter unifier at all. They can always be normalised to non-`let`-expressions.
```haskell
unify Let {} _ = error "unifier: left argument is a let-expression"
unify _ Let {} = error "unifier: right argument is a let-expression"

unify LetRec {} _ = error "unifier: left argument is a let-expression"
unify _ LetRec {} = error "unifier: right argument is a let-expression"
```

And the last case is fired after all other fail, which means that types don't match.
```haskell
unify a b = die $ Mismatch a b
```

## Normalization

Before we can start with inference, there is another matter to discuss. In simpler type systems the type are always in their normal form, there is no abstraction that disobeys `f x ~ g x -> f ~ g` rule, which means that there is no type lambdas.

We have no such restrictions here. There _can_ be lambdas, which gives us a need to evaluate type expression before using them in type checks.

However, the normalization is easy, since the expression to be normalised will be typechecked _before_ it enters normaliser, which kinda gives us moral right to ignore all possible "runtime" failures in normalisation.

I will mostly skip the description of it. The single problem I met is a representation of types. I made a separate constructor for that:
```haskell
Axiom :: Name -> self -> AST_ self
```
This constructor can only be inserted by normalizer, by evaluating, for instance
```haskell
let
  data Bool where
    | True  : Bool
    | False : Bool
in
  <ctx>
```

The names `Bool`, `True` and `False` will be replaced in `ctx` with (in pseudosyntax)

```
axiom "True" Bool
axiom "True" Bool
axiom "Bool" Type
```

accordingly.

## Type Inference

Again, the inference will consist of 2 functions: first one does the work, second one calls the first one _and then applies all substitutions to the program_. Why would we do that? Well, after we finish, there still can be free variables in the program, which means that perhaps some types are not fixed, and this is might cause trouble in the codegen phase.

The only thing is missing is a typing context, which is usually denoted as capital greek Gamma letter. In our case, it will be a map between names and their types.

We will now look at the first function of the two:

```haskell
infer
  :: ( Unifies m
     , HasSubst m
     , HasContext m
     , Refreshes m
     )
  => AST
  -> Sem m AST
```

The free variable can have any type they want, so it can be unified as needed.
```haskell
infer (Var n) = Var <$> refresh n
```

On the contrary, the bound variables can only be typed, if they are inside the context, which should prevent "skolem, rigid variable escaped its scope" situation. The `find` function throws an error unless the variable is present.
```haskell
infer (Bound n) = find n
```

For lambdas, we typecheck and normalise its argument's type and check the body, keeping it in the context.
```haskell
infer (Lam n t b) = do
  _ <- check t Star                  -- The arguments should have kind `Star`.
  let t' = eval t                    -- We reduce the type here, after tc;
                                      -- this way, eval shouldn't fail.
  tb <- withContext [(n, t')] do     -- We infer body under the binder.
    infer b

  return $ Pi n t' tb
```

The same is done for dependent function type. Its type is always `Type`, but we need to check if argument and body are type-correct
```haskell
infer (Pi n t b) = do
  _ <- check t Star
  let t' = eval t
  _ <- withContext [(n, t')] do
    check b Star

  return $ Star
```

Here is our `Type : Type` axiom.
```haskell
infer Star = return $ Star
```

I'll be honest with you: I tried to infer the type of `f` to be a dependent function here. However, this did not want to work at all. Therefore, I just _assume_ that `f` is know to be function here.
```haskell
infer (App f x) = do
  infer f >>= \case
    Pi n t b -> do
      tx <- infer x
      _  <- unified t tx
      applyBindings (instantiate n x b)

    tf -> do
      die $ ExpectedForall tf
```

In case of pattern match, it is just a boring (and therefore, omitted) zip-style descent into both match subject and each pattern, with collection of object parts that structurally match names in the pattern.
```haskell
infer (Match o alts) = do
  t  <- infer o
  ts <- for alts $ inferAlt o
  r  <- refresh "r"
  foldM unified (Var r) ts
```

Records and their types are quite boring as well.
```haskell
infer (Record ns) = do
  ds <- for ns \case
    Val n t b -> do
      tb <- infer b
      t' <- unified t tb
      return $ TDecl n t'

    Data {} -> error "data in record"  -- Because reasons.

  return $ Product ds

infer (Product ts) = do
  for_ ts \(TDecl n t) ->
    check t Star

  return Star
```

For the record access, I again _assume_ that the type is known to be a record, because we don't have any typeclasses to emit `HasType "foo" x` constraint.
```haskell
infer (Get p n) = do
    infer p >>= \case
      Product ds -> do
        case findTDecl n ds of
          Just t -> applyBindings t
          _      -> die $ ExpectedRecordToHaveField n (Product ds)

      other -> do
        die $ ExpectedRecord other
```

For the let-bindings, since both their types and bodies are independent of the declaration itself, we infer them before entering the context. We also check  that types of constuctors in datatypes are well-typed _and_ return an object of type being declared. The type of the binding is the type of the context of the binding.
```haskell
infer (Let d b) = do
    ns <- case d of
      Val n t b' -> do
        tb <- infer b'
        t' <- unified (eval t) tb
        return [(n, t')]

      Data n args ctors -> do
        for_ args \(TDecl _ t) -> do
          infer t

        let k = telescope args Star
        check k Star
        nts <- for ctors \case
          Ctor n' t -> do
            let t' = telescope args t
            _ <- withContext
                ((n, eval k) : [(n'', eval t'') | TDecl n'' t'' <- args])
              do
                check (telescope args t) Star
                ret <- getCtorReturnType t
                ctorCheckReturnType n args ret
            return [(n', t')]
        return $ (n, k) : concat nts

    withContext ns do
      infer b
```

In case of mutually-recursive definitions, theor bodies (but not the types!) might depend on each other, so we have to bind the constructors to whatever is present in the declarations and do the inference only _after_ we enter the block context.
```haskell
infer (LetRec ds b) = do
    ns <- concat <$> for ds \case
      Val n t _ -> do
        check t Star
        return [(n, eval t)]

      Data n args ctors -> do
        nts <- for ctors \(Ctor n' t) -> do
          return [(n', telescope args t)]
        return $ (n, telescope args Star) : concat nts

    withContext ns do
      for_ ds \case
        Val n _ b' -> do
          t  <- find n
          t' <- infer b'
          unified (eval t) t'
          return ()

        Data n args ctors -> do
          for_ ctors \case
            Ctor _ t -> do
              withContext [(n'', eval t'') | TDecl n'' t'' <- args] do
                check (eval (telescope args t)) Star
                ret <- getCtorReturnType t
                ctorCheckReturnType n args ret
              return ()

      infer b
```

There are other cases, but they are either trivial (literals) or the type is contained inside the program directly ("axioms").

## Conclusion

The presence of the typechecker not only allows you to "prevent programs from crashing", but also - if the type system is powerful enough - to partially shift your work onto compiler, by forcing it to implicity calculate some types and, potentially, generate code from them.

In case of dependent types, the type system is very flexible, increasing the possibilities.

There is an [implementation](https://github.com/Heimdell/f-omega) you can `stack install` and play with.

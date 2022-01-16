# TypeScript for Haskellers

Web apps are a mandatory part of every modern application nowadays, no matter how small or complex it is.
From one-click apps that convert pictures to Photoshop, everyone wants fast and easy access to the app, and the web is one of the easiest ways to do that.

In Serokell, we use [TypeScript](https://serokell.io/blog/why-typescript) for writing web applications.
But our main programming language is Haskell. And in this article, we want to show how Haskell knowledge can help you write TypeScript code.

We won't cover all the similarities or differences.
We will also not say much about the differences of TypeScript's type system vs. Haskell's or how it works under the hood.
Our main goal is to show how you can use types in TypeScript like a Haskeller.

## Why TypeScript?

Historically, any new language that allows you to write web applications translates its code to JavaScript.
And there are a lot of such languages and language extensions: TypeScript, Kotlin JS, CoffeeScript, Scala JS, Babel, ClojureScript, etc.
Generally, you can write web applications in almost every language.

Because we like Haskell, could we write our web applications in a functional language (or even in Haskell)? The answer is yes.
There is strongly-typed PureScript with Haskell-like syntax, purely functional Elm, Reflex framework for Haskell with GHCJS, F# with Fabel.

Unfortunately, these don't give us proper flexibility in working with JS libraries (whose numbers are enormous), and when problems related to library management appear, it takes more time to fix those.
Additionally, customers will be locked in when they need to maintain such applications.
On the other hand, TypeScript, which has a vast community and provides modern technologies out of the box, is a great decision.

## Basic concepts

In this part, we will introduce you to simple built-in concepts familiar to most Haskellers that do not require any overhead when writing code.
Nevertheless, we will also leave links to some more complex libraries and articles to help you investigate further.

### Strictness

Unlike Haskell, TypeScript does not focus on types.
They are just there to help programmers write stricter and more understandable code, making their life easier.
You can use types like [`any`](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#any), and nobody pushes you into strict frameworks.

Nevertheless, with a bunch of compiler options, you can make TypeScript more strict in types and other things.
To do that, you need to add the [`strict`](https://mariusschulz.com/blog/the-strict-compiler-option-in-typescript) compiler option to your `tsconfig.json` file.
It will enable all strictness-related flags for you, and if in the future something will be added to the strict options list, you will not need to add it manually.

But in case you want to specify them one-by-one, here are all the current ones: `alwaysStrict`, `strictNullChecks`, `strictBindCallApply`, `strictFunctionTypes`, `strictPropertyInitialization`, `noImplicitAny`, `noImplicitThis`, `useUnknownInCatchVariables`.

### Type aliases and newtypes

#### Type aliases

Type aliases work the same as in Haskell – you can create a new name to refer to some type:

```typescript
type Email = string;
```

And you can also create types with properties:

```typescript
type Document = {
  name: string;
  athor?: string;
}
```

Symbol `?` here is used as syntactic sugar for the `string | undefined` type.

#### Newtype

Creating a `newtype` is not so trivial.
There is no such analogue in TypeScript, but you can do it by using tags.

```typescript
type Email = string & { readonly __tag: unique symbol };
type City = string & { readonly __tag: unique symbol };
```

Here's how all this magic works:

* With [intersection types](https://www.typescriptlang.org/docs/handbook/unions-and-intersections.html#intersection-types) (and syntax), you can add a `tag` field to your type.
* `symbol` lets you declare const-named properties on types.
* `unique` means that the type is unique – your type is tagged with a unique symbol.

With [`as`](https://www.typescriptlang.org/docs/handbook/basic-types.html#type-assertions) (which does type assertion), you can cast your base type to a tagged type.
And when you try to pass a value of another tagged type, the compiler says that the two unique symbols are different.

```typescript
function sendMessage(email: Email);

sendMessage("message"); // Argument of type 'string' is not assignable to parameter of type 'Email'.
sendMessage("St. Petersburg" as City); // Argument of type 'City' is not assignable to parameter of type 'Email'.

sendMessage("email@gmail.com" as Email); // Ok
```

While this is the simplest solution for newtypes, it is not the best for complex systems. 
For a better (but more complex) one, check this [comment](https://github.com/Microsoft/TypeScript/issues/4895#issuecomment-401067935) in TypeScript's issues.

### Algebraic data types

In Haskell, [algebraic data types (ADTs)](https://www.youtube.com/watch?v=UqwLn2OyQ_E) are a commonly used functionality of the language.
They allow you to build your own types from small blocks.
And with pattern matching, it is easy to access this data.

Unfortunately, you can't build an ADT in the same way in TypeScript, but we will show you what we can do with the existing type system. 
We will start with the easiest enums and move to more complex things after that.

#### Union types

Union types allow you to combine different types.
Unions are not tagged, so they are just a set of possible types.

```typescript
type Result = string | number | (() => string);
```

Here if you already failed to match some type, the compiler knows that it will not be present in the future.
In the first `if`, we passed `typeof result === "number"`, so we know that it may be only `function` or `string` in the code below.
And we can run `length.toString()` on this value (even if `number` doesn't have such a property) since both `function` and `string` have property `length`.
But we can't `call` this value because `string` is not callable. We can do it only after failing to match `string`.

```typescript
const resultInterpreter = (result: Result): string | undefined => {
  if (typeof result === "number") {
      return result.toFixed();
  }

  // Okay: return result.lenght.toString(); 
  // Error: return result();

  if (typeof result === "string") {
      return result.toLocaleUpperCase();
  }

  return result();
};
```

#### Unit types

The next feature is unit types.
Unit types are a subtype of primitive types that contain precisely one primitive value.
With unions, you can use it like [enums](https://www.typescriptlang.org/docs/handbook/enums.html) by setting unique string values.
Or even use values with different types.

```typescript
type Result = true | "error" | 5;
```

You can pass it to the function and match it by its type.

```typescript
const resultInterpreter = (result: Result): string => {
  if (result === true) {
    return "true";
  }
  if (result === "error") {
    return result;
  }
  return "five";
};
```

#### Discriminated unions, or a data type analogue

Going further with unions makes it possible to construct an analogue of Haskell data types.
Those are called discriminated unions.
The main idea is to store a unique property for each variant that will allow switching on it.

```typescript
type Result = 
  | {type: "error"; message: string}
  | {type: "result"; value: number}

const resultInterpreter = (result: Result): string => {
  switch (result.type) {
    case "error":
      return result.message;
    case "result":
      return `valid ${result.value.toString()}`;
    default:
      return "impossible";
  }
};
```

#### Pattern matching

These were straightforward examples of working with ADT-like structures in TypeScript.
By using type parameters, you can also make some more complex things like [this library](https://github.com/pfgray/ts-adt) for ADTs.
But one of the most valuable features of Haskell that is connected with ADTs is pattern matching.
It allows you to disclose you structure comparing it with some pattern and based on this condition, execute some code.
There is [no such functionality](https://github.com/tc39/proposal-pattern-matching) in TypeScript yet, but [ts-pattern](https://github.com/gvergnaud/ts-pattern) can provide it for you.

Example from repository:

```typescript
type Data =
  | { type: 'text'; content: string }
  | { type: 'img'; src: string };

type Result =
  | { type: 'ok'; data: Data }
  | { type: 'error'; error: Error };

const result: Result = ...;

return match(result)
  .with({ type: 'error' }, (res) => `<p>Oups! An error occured</p>`)
  .with({ type: 'ok', data: { type: 'text' } }, (res) => `<p>${res.data.content}</p>`)
  .with({ type: 'ok', data: { type: 'img', src: select() } }, (src) => `<img src=${src} />`)
  .exhaustive();
```

### Immutability

#### `const` and `readonly`

TypeScript has two base primitives to work with immutability: `const` and `readonly`.

The first one is used to prevent variable reference change.

```typescript
const a = 1;
a = 2; // Cannot assign to 'a' because it is a constant.
```

And the second one is used to make properties immutable.

```typescript
interface A {
  readonly x: number;
}
const a: A = { x: 1 };
a.x = 12; // Cannot assign to 'x' because it is a read-only property.
```

Nevertheless, you can easily see problems here.
`const` and `readonly` only block reference changes but do nothing about values.
With `const a = [1, 2, 3]` or `readonly x: number[]`, you still will be able to change the content of an array.

Also, you need to be careful when passing an object with read-only fields to a function because it may be changed inside the function.
This happens because of information loss.
The type of an object with `readonly` is a subtype of the type without it.
When you pass it to a function with a more general type, the function does not have access to information about its subtype.
As a result, you should always write proper types of arguments.

```typescript
const a: { readonly x: number } = { x: 1 };

const changeA = (arg: { x: number }): void => {
  const argC = arg;
  argC.x = 2;
}

changeA(a);
a.x; // 2
```


#### `Readonly` types

With the `Readonly` type, you can mark all properties as `readonly`.

```typescript
type A = {
  x: number;
  y: number;
};

type ImmutableA = Readonly<A>; 
const a: ImmutableA = { x: 1, y: 2 };
a.x = 2; // Cannot assign to 'x' because it is a read-only property.
a.y = 1; // Cannot assign to 'y' because it is a read-only property.
```

With `ReadonlyArray`, you can make all array elements `readonly`.

```typescript
const arr: ReadonlyArray<number> = [1, 2, 3];
arr[0] = 4; // Index signature in type 'readonly number[]' only permits reading.
```

You can also make a `readonly` dictionary, for example:

```typescript
interface A {
    readonly [x: string]: number;
}

const a: A = { x: 1, y: 2 };
a.x = 2; // Index signature in type 'A' only permits reading.
a.y = 1; // Index signature in type 'A' only permits reading.
```

Now you have a more flexible way to make things read-only.

#### Working with immutable objects

Although you can now get immutable objects, usually you don't need them as they are.
You want to do some actions on them and make new immutable objects.
To do that, you can construct new objects by hand or use the `Writable` utility type, but it is not that simple.

Fortunately, there are libraries that can help you deal with this issue.

The first one (and the most powerful) is [immer](https://immerjs.github.io/immer/), which is used for immutable state flow.
Immutable data there is not copied but shared in memory.
In other words, `immer` gives you the ability to work with a draft copy of your data and not worry about mutability.
After all changes, it will produce an actually immutable state.
It also has helpers for React.

Produce here is a base primitive for working with `immer`.
It takes your current state and a function, which takes a draft and applies changes.
And in the end, it creates a new `nextA` state without any changes in  `a`.

```typescript
type A = Immutable<
  {
    text: string;
    valid: boolean;
  }[]
>;

const a: A = [
  {
    text: "valid text",
    valid: true,
  },
  {
    text: "invalid text",
    valid: false,
  },
];

const nextA = produce(a, (draft) => {
  draft[1].valid = false;  // set valid = false for the second element
  draft.push({ text: "some text", valid: false }); // add new element to array
});
```

And finally, there are three interesting optics libraries that can help you work with immutable data structures: 

* [`optics-ts`](https://github.com/akheron/optics-ts). The newest and more TypeScript-oriented optics library.
* [`monocle-ts`](https://github.com/gcanti/monocle-ts). This one supports TypeScript and is a partial porting of Scala monocle.
* [`partial.lenses`](https://github.com/calmm-js/partial.lenses). This one, unfortunately, doesn't have type bindings.


### Higher-order functions and currying

Many languages support higher-order functions, and TypeScript is not an exception.
You can use functions as an argument and also return them.

```typescript
const filterArray = (fn: (elem: string) => boolean, arr: string[]): string[] => {
  const newArray: string[] = [];
  for (let i = 0; i < arr.length; i++) {
    if (fn(arr[i])) {
      newArray.push(arr[i]);
    }
  }
  return newArray; // or just simply return arr.filter(fn);
};

const f = (x: string): ((s: number) => boolean) => {
  return (s: number) => {
    return s.toString() === x;
  };
};

f("5")(5) // true;
```

As you may see in the last example, we can call the function `f` and return a function that takes another argument.
It's definitely possible to implement some kind of currying through this.

You can split the type signature of the function from its implementation.
Unfortunately, unnamed parameters are not supported, but you can set them as `_`.

```typescript
type addT = (_: number) => (_: number) => number;
const add: addT = (l) => (r) => l + r;

type foldArrT = (f: (_: number) => (_: number) => number) => (_: number) => (_: number[]) => number;
const foldArr: foldArrT = (f) => (z) => (arr) => {
  let m = z;
  arr.forEach((elem) => {
    m = f(m)(elem);
  });
  return m;
};

foldArr(add)(0)([1, 2, 3, 4, 5]) // 15;
```

The syntax is weird since you always need to use named parameters.
But it is still possible, and you can use it in your code quite easily.

### Polymorphism

There are different kinds of polymorphisms, but we will cover only three of them: parametric, ad-hoc, and row polymorphism.

Let's start with the first one.

#### Parametric polymorphism

Parametric polymorphism allows us to write abstract functions or data types which don't depend on their type.
In TypeScript, we call those generics.

```typescript
const arrLength: <T>(_: T[]) => number = (arr) => arr.length;

arrLength([1, 2, 3]); // 3
arrLength<string>(["1", "2"]); // 2
arrLength<number>(["1", "2", "3"]); // Type 'string' is not assignable to type 'number'.

type NonEmpty<T> = {
  head: T;
  tail: T[];
};
```

#### Ad-hoc polymorphism

Ad-hoc polymorphism allows you to implement abstract functions, the logic of which will be different with different types.

Let's define an `Eq` interface: 

```typescript

interface Eq<T> {
  equal: (f: T, s: T) => boolean;
}
```

And now we will create two implementations of it: `IntEq` for numbers and `IntArrEq` for arrays of numbers.

The first one will simply compare two numbers, and the second one will return equality if the first array contains all the elements from the second array.

```typescript
class IntEq implements Eq<number> {
  equal(f: number, s: number) {
    return f === s;
  }
}

class IntArrEq implements Eq<number[]> {
  equal(f: number[], s: number[]) {
    return f.filter(item => s.indexOf(item) < 0).length === 0;
  }
}
```

Now let's implement a `lookup` function, which for an array of key-value pairs and a specific key will return the value associated with this key or `undefined`.

We can write it polymorphic by passing a comparator class that implements `Eq` for the key type.

```typescript
const lookup = <T, K extends Eq<T>, V>(cmp: K, k: T, mp: [T, V][]): V | undefined => {
  let result: V | undefined;
  mp.forEach(([kk, v]) => {
    if (cmp.equal(k, kk)) {
      result = v;
    }
  });
  return result;
};

lookup(new IntEq(), 1, [[1, "2"], [2, "1"]]); // "2"
lookup(new IntArrEq(), [1, 2, 3], [[[1, 3], "1"], [[3, 2], "2"], [[2, 1, 3], "3"]]); // "3"
```


#### Row polymorphism

The last kind of polymorphism is row polymorphism.
It allows you to ensure that a record contains at least the given set of fields.

Unfortunately, TypeScript's type compatibility is based on structural subtyping, so what we have available is not _exactly_ row polymorphism.
Despite this, we can use intersection types to emulate this.
But you should be careful with using it since you can lose information during assignments.
Therefore, you must be cautious about propagating the full type-level information when you need it.

```typescript
type fnT = <T>(v: T & { x: number }) => T & { x: number }
// You should fully describe all types to not lose information.

type A = {};
type B = A & {x: number};

let a: A = {};
const b: B = {x: 1};
a = b;
a.x; // Property 'x' does not exist on type 'A'.
```

### Mapped types, conditional types, and type families

[Mapped](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html) and [conditional](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html) types are a rather practical part of TypeScript.
They give us the flexibility to modify existing types or create new ones out of them.

#### Mapped types

Mapped types allow you to create new types based on old types by transforming them in some way.
The syntax looks like this: `[set of field names]: type;`.
For example, you can add an optional modifier or a readonly modifier to each field with `type Partial<T> = { [P in keyof T]?: T[P] }` and `type Readonly<T> = { readonly [P in keyof T]: T[P] }`, respectively.

```typescript
type Partial<T> = { [P in keyof T]?: T[P] };

type A = { x: number; y: number };
type PartialA = Partia<A>;

const a: A = { x: 1 }; // Property 'y' is missing in type '{ x: number; }' but required in type 'A'.
const a: PartialA = { x: 1 }; // Ok
```

In the code above: 

* `Partial` type adds the `?` modifier for each property.
* The `keyof` operator in the code above produces a union of properties name.
* Square brackets indicate the computed names.
* And the `in` operator goes over all the field keys.

It is also possible to remove such modifiers using `-?` and `-readonly`. Let's try to do that. 

```typescript
type NoModifiers<T> = { -readonly [P in keyof T]-?: T[P] };

type A = { readonly x?: number; readonly y?: number };
type NoModifiersA = NoModifiers<A>;

const a: A = { x: 1 }; // Ok
a.x = 1; // Cannot assign to 'x' because it is a read-only property.

const a: NoModifiersA = { x: 1 }; // Property 'y' is missing in type '{ x: number; }' but required in type 'NoModifiers<A>'
const a: NoModifiersA = { x: 1, y: 2 }; // Ok
a.x = 2; // Ok
```

Now, let's look at some more complex examples of mapped types.

First, define a type that contains string and numerical properties.

```typescript
type Point = {
  x: number;
  y: number;
  0: string;
}
```

Now, using mapped types, we will create a type that will pick only some of the properties.
Word `extends` here says that generic `K` will contain a subset of union properties keys `T`.
And the resulting type will contain only properties from `K`.

```typescript
type Pick<T, K extends keyof T> = {
  [P in K]: T[P];
};

type PointValues = Pick<Point, "x" | 0> // type PointValues = { x: number; 0: number }
```

The next example will be more complex.
We will try to create the type which produces a type with getters for all string properties.

To implement it, let's first define a `Getter` helper type.
Capitalize is a built-in type, which may be applied to the string type.
So, for `T`, which extends string, we can produce getter specific name.

```typescript
type Getter<T extends string> = `get${Capitalize<T>}`;
```

And finally, we can implement the `Getters` type.
Type `Extract` extracts from a type all union members that are assignable to some type.
So, here for each property name `K` in `T` that is assignable to `string`, we create a getter property name with type `() => T[K]`.

```typescript
type Getters<T> = {
    [K in keyof T as Getter<Extract<K, string>>]: () => T[K];
}

type PointGetters = Getters<Point> // type PointValues = { getX: () => number; getY: () => number; }
```

#### Conditional types

Another great feature of TypeScript is conditional types.
It allows you to choose the type based on some condition expressed as another type.

With the `extends` keyword, you can make a condition: `T extends U ? X : Y`.

Here is a basic example of a conditional type.
Type `B` extends `A`, but type `C` doesn't, and the `Condition` type will return one type if `T extends V` and the other one otherwise.

```typescript
type A = { x: number };
type B = { x: number; y: number };
type C = { y: number };

type Condition<T, V> = T extends V ? number : string;

type N = Condition<B, A>; // type N = number
type E = Condition<C, A>; // type E = string
```

It also allows you to choose not only from two but from many types: `T extends U ? X : T extends W ? Y : Z`.

With such functionality, you can implement things that resemble [type families](https://serokell.io/blog/type-families-haskell), although they are not quite like them. In conjunction with mapped types, it is a great feature to manage your types.

Conditional types together with union types are called distributive conditional types. How they work might be a little bit confusing, but we'll give an example.

```typescript
type Exclude<T, U> = T extends U ? never : T;
```

The `Exclude` type may look strange, but what if we will pass union types here?
It will simply exclude such types in union `T` which exist in union `U` by setting `never` type, where the [`never`](https://www.tutorialsteacher.com/typescript/typescript-never) type describes the type of values that will never occur.

You may think about it as two nested cycles.
For each type in `T` and each type in `U`, if `T extends U`, set `never`. Otherwise, set `T`.

```typescript
type A = "a" | "b" | "c";
type B = Exclude<A, "a" | "b">; // type B = "c"
type C = Exclude<A, "a" | "b" | "c">; // type C = never
type D = Exclude<A, "z">; // type D = "a" | "b" | "c"
```

With distributed conditional types, we can write an `Omit` type which will remove properties from a type.
When taking properties of `T` via `keyof T`, we exclude properties whose names are written in `K`, and then simply pick the remaining ones using `Pick`.

```typescript
type Omit<T, K> = Pick<T, Exclude<keyof T, K>>;

type Point = {
  x: number;
  y: number;
  z: number;
}

type PointX = Omit<Point, "y" | "z">; // type PointX = { x: number }
```

TypeScript [contains](https://www.typescriptlang.org/docs/handbook/utility-types.html) a lot of predefined conditional and mapped types that you can use out of the box.

## Going further

Built-in concepts are good.
But as you can see, TypeScript's type system is not really a weak one.
By adding our knowledge from Haskell and type theory, we can get more from this type system.
This part will show very brief descriptions of different approaches. We have also added links to references and other articles to help you understand them more deeply, if necessary.

### HKTs

As we said before, TypeScript's type system is not so bad as one may think.
But it has one significant limitation – the absence of kinds.
Higher-kinded types allow us to write types that have their own type constructors as parameters.
So, with them, we can bring another level of abstraction.
Imaging that your generic type is not just specific type, but type constructor like `Map` or `Array` which is waiting for its own generic type to be specified.

Let's take a look on example of theoretical implementation, which, unfortunately, TypeScript's type system doesn't allow.

```typescript
interface Collection<F> {
  create: <A>() => F<A>; // Type 'F' is not generic.
  insert: <A>(v: A) => (c: F<A>) => F<A>; // Type 'F' is not generic.
}

const collectionArray: Collection<Array> = {
  create: () => [],
  insert: (v) => (c: number[]) => [...c, v]
};
```

Fortunately, we can simulate kinds by using defunctionalization, which allows us to translate higher-order programs into a first-order language.
The main idea is to map type constructor names to their implementations.
With it, we can create a type Kind which works with `* -> *` constructors, Kind2 for `* -> * -> *`, and so on.

Let's define two types: `URItoKind` and `URItoKind2`.
They will be our identifiers for 1-arity and 2-arities types.
And `URIS` with `URIS2` will be present for all such types.

```typescript
type URItoKind<A> = {
  'Array': Array<A>;
  'Set': Set<A>;
}

type URItoKind2<A, B> = {
  'Map': Map<A, B>;
}

type URIS = keyof URItoKind<unknown>;
type URIS2 = keyof URItoKind2<unknown, unknown>;
```

Here, [`unknown`](https://mariusschulz.com/blog/the-unknown-type-in-typescript) is a more type-safe representation of `any` that forces us to do some checks before doing actions with values of this type.

So, now we are ready to present our kinds.
They take an identifier property as the first type parameter and rest parameters are for type parameters of this identifier.

```typescript
type Kind<F extends URIS, A> = URItoKind<A>[F];
type Kind2<F extends URIS2, A, B> = URItoKind2<A, B>[F];
```

And now we can return to the `Collection` above and implement it using kinds.

```typescript
interface Collection<F extends URIS> {
  create: <A>() => Kind<F, A>;
  insert: <A>(v: A, c: Kind<F, A>) => Kind<F, A>;
}

const collectionArray: Collection<"Array"> = {
  create: () => [],
  insert: (v, c) => [...c, v]
};
```

Let's see how it works in practice.

We implement a new `Collection` instance with `Set`, and then write a generic function `f` over `Collection`.
The last thing we need to do is to pass the implementation as a function argument.

```typescript
const collectionSet: Collection<"Set"> = {
    create: () => new Set(),
    insert: (v, c) => c.add(v),
};

const f = <F extends URIS, A>(C: Collection<F>, v1: A, v2: A) => {
    const newCollection = C.create<A>();
    return C.insert(v2, C.insert(v1, newCollection));
};

f(collectionArray, 1, 2); // [ 1, 2 ]
f(collectionSet, 2, 2); // Set(1) { 2 }
```

Of course, this is a tiny subset of what you can do with kinds.
Most of this part is inspired by [`fp-ts`](https://gcanti.github.io/fp-ts/) library and [Yuriy Bogomolov's](https://ybogomolov.me/) blog.
`fp-ts` contains a ton of things that you may know from Haskell, and in the blog, you can find excellent explanations of how the library works.

There are a lot of libraries based on `fp-ts`: 

* [`io-ts`](https://github.com/gcanti/io-ts);
* [`parser-ts`](https://github.com/gcanti/parser-ts); 
* [`monocle-ts`](https://github.com/gcanti/monocle-ts); 
* [`remote-data-ts`](https://github.com/devex-web-frontend/remote-data-ts);
* and others.

### Peano numbers

Moving further, we can look into type-level programming and, more concretely, about computations on types.
The basic primitive which may help us with this problem is Peano numbers.
Let's see how those would look in TypeScript.

```typescript
type Zero = "zero";

type Nat = Zero | { n: Nat };
```

And now we are able to define `Succ`, which adds one to our `Nat`.

```typescript
type Succ<N extends Nat> = { n: N };
```

With a simple `Nat` definition, we can make something useful.

Let's create a type-safe vector that will store its length in type.

Empty vector will simply be a `"nil"`.

```typescript
type Nil = "nil";
```
Type `Cons` will add a value to the vector and increase its type-level length.

```typescript
type Cons<A, N extends Nat> = {a: A, v: Vec<A, N>};
```

And the main type, `Vec`, combines them by using a conditional type.

```typescript
type Vec<A, N extends Nat> = N extends Succ<infer R> ? Cons<A, R> : Nil;
```

Using `infer`, we are able to infer a new type variable `R` from `N`.
So, when `N` is a `Succ`, we can infer nested `R` from it and return `Cons<A, R>`.
Otherwise, it is `Nil`.

And here are examples of `Vec`'s usage.
We define two helper functions that build `Vec` values.
And, as you can see, we can use it safely.

```typescript
const emptyVec: <A>() => Vec<A, Zero> = () => "nil";

const pushVec: <A, N extends Nat>(a: A, v: Vec<A, N>) => Vec<A, Succ<N>> = (a, v) => {
  return {a, v}
};

let empty: Vec<number, Zero> = emptyVec(); // Ok
let oneElem: Vec<number, Succ<Zero>> = pushVec(1, empty); // Ok
let twoElems: Vec<number, Succ<Succ<Zero>>> = pushVec(2, oneElem); // Ok
let twoElemsInvalid: Vec<number, Succ<Zero>> = pushVec(2, oneElem); // error
oneElem = twoElems; // error
```

This was a simple example of what we can do with type-level programming.
Using such primitives, we can implement different operations on types and even [type-level Fibonacci](https://mjj.io/2021/03/29/type-level-programming-in-typescript/), for example.

```typescript
type Fibonacci<N, F0 = Zero, F1 = One> = {
  acc: F0
  n: N extends Succ<infer _> ? Fibonacci<Decrement<N>, F1, Add<F0, F1>> : never
}[IfElse<Equals<Zero, N>, "acc", "n">]
```

### GADTs and eDSLs

#### Generalized algebraic data types

Generalized algebraic data types (GADTs) in Haskell give us the ability to manually write types of constructors.
With a data type `D a` with the type value `a`, we can create constructors like `C :: Int -> D Int`.

The key feature here is that Haskell can infer the equality of types `a` and `Int`.
TypeScript, on the other hand, doesn't have equality inference.
Nevertheless, we can manually provide such equality to it as a value.

The other great feature of this approach is that we can make a nice eDSL with GADTs.
TypeScript will not provide you with such a good eDSL as Haskell would, but it doesn't mean that we can't do it.

Let's show on example how it will look like and work.
We will use `fp-ts` here and below since it has a lot of useful built-in types and functions.

```typescript
import { identity } from "fp-ts/lib/function";
```

The next step is defining `Equality`.
To provide equality as a value, we use Leibniz equality.

Here is a simplified version of it.

Interface `Equality` is a hybrid type, which gives an object the ability to act as a function.
Then in `ArithExpr`, we simplify `Equality<A, B>` into an identity `(a: A) => A`.
And, finally, in `interpret`, switching on `type` TypeScript does type narrowing for that case, and `proof` gets inferred into a conversion of that specific type into `A`.
To fully understand this equality, you can check the [original](http://code.slipthrough.net/2016/08/10/approximating-gadts-in-purescript/) PureScript article and also Giulio Canti's [example](https://gist.github.com/gcanti/9a0c2a666621f03b80457831ff3ab997) (on which this part is based on) of its implementation in TypeScript.

```typescript
interface Equality<A, B> {
  (a: A): B;
}
```

Now we can use `Equality` to write our GADT with additional proof representation as a value.

```typescript
type ArithExpr<A> =
  | { type: "Num"; v: number; proof: Equality<number, A> }
  | { type: "Plus"; l: ArithExpr<number>; r: ArithExpr<number>; proof: Equality<number, A> }
  | { type: "Gt"; l: ArithExpr<number>; r: ArithExpr<number>; proof: Equality<boolean, A> }
  | { type: "And"; l: ArithExpr<boolean>; r: ArithExpr<boolean>; proof: Equality<boolean, A> };
```

Now let's define our helper functions which will help us to build expressions and use `identity<A>(a: A): A` as a proof.

```typescript
const num: (v: number) => ArithExpr<number> = (v) => {
  return { type: "Num", v, proof: identity };
};

const plus: (l: ArithExpr<number>, r: ArithExpr<number>) => ArithExpr<number> = (l, r) => {
  return { type: "Plus", l, r, proof: identity };
};

const gt: (l: ArithExpr<number>, r: ArithExpr<number>) => ArithExpr<boolean> = (l, r) => {
  return { type: "Gt", l, r, proof: identity };
};

const and: (l: ArithExpr<boolean>, r: ArithExpr<boolean>) => ArithExpr<boolean> = (l, r) => {
  return { type: "And", l, r, proof: identity };
};
```

The last step will be just the implementation of `interpret`.
With a switch statement on `expr.type`, we construct our result by running `interpret` on nested expressions and proving resulting types with the proof from this `expr`.

```typescript
const interpret: <A>(expr: ArithExpr<A>) => A = (expr) => {
  switch (expr.type) {
    case "Num":
      return expr.proof(expr.v);
    case "Plus":
      return expr.proof(interpret(expr.l) + interpret(expr.r));
    case "Gt":
      return expr.proof(interpret(expr.l) > interpret(expr.r));
    case "And":
      return expr.proof(interpret(expr.l) && interpret(expr.r));
    default:
      return expr;
  }
};

const testExpr = and(gt(plus(num(23), num(12)), num(170)), gt(num(35), num(47)));
interpret(testExpr); // false

const wrongExpr = and(num(23), num(12)); // Argument of type 'ArithExpr<number>' is not assignable to parameter of type 'ArithExpr<boolean>'.
```

#### Tagless final eDSL

Another way to implement an eDSL is [tagless final](https://serokell.io/blog/introduction-tagless-final).
We can move from data type to type classes having the same logic.
Here type class will describe possible actions and using instances we will be able to write interpreters.

Let's start with defining our type class.
To do this, we need the already-mentioned `URIS`, which is a union of all 1-arity defined in `URItoKind`, and also the already-mentioned `Kind`.

```typescript
import { Kind, URIS } from "fp-ts/HKT";

type ArithExpr<Expr extends URIS> = {
  num: (v: number) => Kind<Expr, number>;
  plus: (l: Kind<Expr, number>, r: Kind<Expr, number>) => Kind<Expr, number>;
  gt: (l: Kind<Expr, number>, r: Kind<Expr, number>) => Kind<Expr, boolean>;
  and: (l: Kind<Expr, boolean>, r: Kind<Expr, boolean>) => Kind<Expr, boolean>;
};
```

Now, we will present our types for which we will create `ArithExpr` instances.
The first one will be just an `Interpreter<A>`.
And the second one `ToS<A>` will create a string representation of expression.
But now we need somehow add them to the `fp-ts` `URItoKind` interface to be able to use them in `ArithExpr` as it take `Expr extends URIS`.
And `URIS` is just `keyof URItoKind<unknown>`.
Here we need the TypeScript module augmentation feature, which allows us to patch existing objects by importing and then updating them.
So, we simply add `Interpreter` and `ToS` to `URItoKind`.

```typescript
type Interpreter<A> = {
  interpret: A;
};

type ToS<A> = {
  toString: string;
};

declare module "fp-ts/lib/HKT" {
  interface URItoKind<A> {
    readonly Interpreter: Interpreter<A>;
    readonly ToS: ToS<A>;
  }
}
```

After all the preparations, we are ready to create instances.

```typescript
const arithInterpreter: ArithExpr<"Interpreter"> = {
  num: (v: number) => {
    return { interpret: v };
  },
  plus: (l: Interpreter<number>, r: Interpreter<number>) => {
    return { interpret: l.interpret + r.interpret };
  },
  gt: (l: Interpreter<number>, r: Interpreter<number>) => {
    return { interpret: l.interpret > r.interpret };
  },
  and: (l: Interpreter<boolean>, r: Interpreter<boolean>) => {
    return { interpret: l.interpret && r.interpret };
  },
};

const arithToS: ArithExpr<"ToS"> = {
  num: (v: number) => {
    return { toString: v.toString() };
  },
  plus: (l: ToS<number>, r: ToS<number>) => {
    return { toString: `(${l.toString} + ${r.toString})` };
  },
  gt: (l: ToS<number>, r: ToS<number>) => {
    return { toString: `(${l.toString} > ${r.toString})` };
  },
  and: (l: ToS<boolean>, r: ToS<boolean>) => {
    return { toString: `(${l.toString} && ${r.toString})` };
  },
};
```

And finally, let's create a test expression and run it with different instances.

```typescript
const testExpr: <Expr extends URIS>(E: ArithExpr<Expr>) => Kind<Expr, boolean> = (E) =>
  E.and(E.gt(E.plus(E.num(23), E.num(12)), E.num(170)), E.gt(E.num(35), E.num(47)));

testExpr(arithInterpreter).interpret; // false
testExpr(arithToS).toString; // (((23 + 12) > 170) && (35 > 47))
```

You can also see more complex examples in Yuriy Bogomolov's [eDSL workshop](https://github.com/YBogomolov/workshop-edsl-in-typescript/tree/master).

## Conclusion

In this article, we have shown how your knowledge from Haskell may help you write type-safe code in TypeScript.
We started from basic concepts such as type aliases and data types and moved towards more complex and TypeScript-specific topics like mapped and conditional types.
In the last part, we learned how to implement more complex things with TypeScript's type system.

This article is not a tutorial for learning TypeScript.
A lot of things were omitted, and others may require more detailed research from you.
To help you with that, we have tried to provide links to external materials.
TypeScript also contains different concepts not only from Haskell but also from other languages and programming paradigms.
So, feel free to study those as well and use them with the already received knowledge.
Nevertheless, this article may help you understand what kind of things you can do with types in TypeScript.

Hopefully, you can use things you learned here, develop your own solutions based on them, and dive into the extraordinary world of TypeScript.

If you would like to read more TypeScript articles, follow us on [Twitter](https://twitter.com/serokell) and [DEV](https://dev.to/serokell), or subscribe to our newsletter below.

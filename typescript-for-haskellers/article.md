# TypeScript for Haskellers

Web app nowadays is mandatory part of every modern application.
It doesn't matter how small or complex it is.
From one click apps with pictures converting to photoshop everyone wants fast and easy access to it and Web is where you can give it to your users or even for you with the easiest way.

Moreover, UI and UX standards are increasing.
People like simple interfaces with enjoyable.
Application with worse functionality and better UI/UX may win on the market against the other one with bad design.

One might say that doesn't matter what your application is about or what part of it you write.
In the end in most of the cases you will need to give access for it to smb.
Whether it is an external user or your colleague using internal products.

So, in Serokell we use TypeScript for writing web pages.
And as our main language is Haskell, in this article we want to explain how knowledge from it will help you to write TypeScript code.

## Why TypeScript?

Basically our Web world consists of 3 things: HTML, CSS, and JavaScript.
The first one is about content, the second one about this content style and the last one about how you can interact with this content.
Moreover, these 3 things are just files, and you need a way to read and show them.
That is what browsers do.

Historically this set of files was perfect to write web pages.
And because of the enormous growth of the web JavaScript in particular has grown enormous as well.

So, as a result any new language which allow you to write web applications translate its code to JavaScript.
And there are a lot of such languages and language extensions: TypeScript, Kotlin JS, CoffeeScript, Scala JS, Babel, ClojureScript, etc.
Generally you can write web applications almost on every language.

As we like Haskell, maybe we can write our Web application on functional languages or even on Haskell? And the answer is yes.
There is strongly-typed PureScript with Haskell like syntax, purely functional Elm, Reflex framework for Haskell with GHCJS, F# with Fabel.

But when we use Haskell we know that people like it and customers want it, and we can't say the same thing about technologies above.
Customers will be locked in this solution as most likely they will not be able to maintenance such application by themselves.
On our side we will be locked in using JS libraries which number is enormous, build problems, and fast team expanding.

Thus, we needed a language which will easy support all modern technologies from web development, big community, and give us a possibility to use our knowledge from Haskell.
And here it is -- TypeScript.
It is static typing language which popularity grows from year to year.
A lot of new libraries are writen on it, a lot of have type declarations, and it provides an easy way to use plain JS libraries as well.

Now it is time to see how your Haskell experience may help you in learning TypeScript and how you can improve your code with it.

In this article we don't have a goal to cover all similarities or differences.
We also will not say about disadvantages of TypeScript type system against Haskell or how it works under the hood.
Our goal is to show what and how you can use types in TypeScript.

## Similar concepts

Unlike Haskell in TypeScript types are not the center part of the language.
They just help programmers to write more strict and understandable code, making their life easier.
You can use types like `any` and nobody pushes you into strict frames.

So, this part will be about simple build in concepts which are familiar for haskellers and which do not require any overheads on writing code.
Nevertheless, we will also leave links for some more complex libraries and articles which may help you or deeply investigate the theme.

### Strictness

The first thing you need to know is that TypeScript was developed to provide an ability for JavaScript developers using it.
So, by default TypeScript is really polite in using types.
Nevertheless, with bunch or compiler options you can make it more strict in types and other things.
So, the main strict compiler option is which you need to add to your `tsconfig.json` file is `strict`.
It will enable all strict related flags for you and if in future smth will be added to strict options list you will not need to add it manually.
But if you want to specify them here are all options: alwaysStrict, strictNullChecks, strictBindCallApply, strictFunctionTypes, strictPropertyInitialization, noImplicitAny, noImplicitThis, useUnknownInCatchVariables.


### Type aliases and newtypes

Type aliases works exactly the same as in Haskell.
You can create a new name to refer to some type.

```typescript
type Email = string;

type Document<T> = {
  name: string;
  athor?: string;
  data: T;
}
```

Creating newtype is not so trivial.
There is no such analog in TypeScript, but you can do it using tags.

```typescript
type Email = string & { readonly __tag: unique symbol };
type City = string & { readonly __tag: unique symbol };

function sendMessage(email: Email);

// type error
sendMessage("message");
sendMessage("St. Petersburg" as City);

// valid
sendMessage("email@gmail.com" as Email);
```

Anyway this is not an ideal solution for newtypes but there is a [comment](https://github.com/Microsoft/TypeScript/issues/4895#issuecomment-401067935) in issue with a better solution and there is also a [library](https://github.com/sveseme/taghiro) which implement it to a nice view.

### Unit types, Unions, Data types and Pattern Matching

Haskell ADT is a constantly used functionality of language.
It allows you to build your own big types from small blocks.
And with pattern matching it is easy to get access to this data.

Unfortunately there is no same way to build ADT, but we will start from the easiest enums and move to the more complex things showing what we can do using existing type system in TypeScript.

Unit types are subtypes of primitive types that contain exactly one primitive value.
You can use it like enums setting unique string values.
Or even use different values types.

```typescript
type Result = true | "error" | 5;

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

Moving forward the next is feature Unions.
Unions are not tagged, so it is just a condition of possible types.

```typescript
type Result = string | number | (() => string);

const resultInterpreter = (result: Result): string | undefined => {
  if (typeof arg === "string") {
    return "error";
  } else if (typeof n === "number") {
    return "valid";
  } else if (typeof arg === "function") {
    return undefined;
  }
}
```

Going deeper using Unions it is possible to construct data type analog.
It is called discriminated Unions.
The idea is to store special property for each variant which will allow switching on it.

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

This is the very simple examples of working with ADT like structures in TypeScript.
Using type parameters you are also able to make some more complex things like [such](https://github.com/pfgray/ts-adt) library for ADTs.
But one of the most useful features of Haskell connected with ADTs is pattern matching.
In allows you to disclose you structure comparing it with some pattern and based on this condition execute some code.
There is [no such functionality](https://github.com/tc39/proposal-pattern-matching) in TypeScript but [ts-pattern](https://github.com/gvergnaud/ts-pattern) will bring it to you.
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

TypeScript has two base primitives to work with immutability: const and readonly.
The first one is used to prevent variable reference change.
And the second one for properties immutability.

```typescript
const a = 1;
a = 2; // error

interface A {
  readonly x: number;
}
const a: A = { x: 1 };
a.x = 12; // error
```

Nevertheless, you can easily see problems here.
Const and readonly only block the reference change, but make nothing about values.
With `const a = [1, 2, 3]` or `readonly x: number[]` you still will be able to change the content of array.
Let's introduce more cases and how you can deal with them in TypeScript.

```typescript
type A = {
  x: number;
  y: number;
};

type ImmutableA = Readonly<A>; 
const a: ImmutableA = { x: 1, y: 2 };
a.x = 2; // error
a.y = 1; // error


const arr: ReadonlyArray<number> = [1, 2, 3];
arr[0] = 4; // error


interface A {
    readonly [x: string]: number;
};

const a: A = { "x": 1, "y": 2 };
a["x"] = 2; // error
a["y"] = 1; // error
```

Also be careful passing object with readonly fields to function as it may cause unexpected behaviour.

```typescript
const a: { readonly x: number } = { x: 1 };

const changeA = (arg: { x: number }): void => {
  const argC = arg;
  argC.x = 2;
}

changeA(a);
console.log(a.x); // 2
```

Now you get more flexible way to make things readonly.
Although now you can get immutable objects usually you don't need them as is.
You want to make some actions on them and make new immutable objects.
But it is not so easy.
You will need to construct new objects by hands, you can also use `Writable` utility type, but it still is not really simple to copy and construct new objects.

Let's introduce some libraries which may help you deal with this issue.
The first one and the most powerful is [immer](https://immerjs.github.io/immer/).
It is used for immutable state flow.
Immutable data is not copied but shared in memory.
It also has helpers for React.

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
  draft[1].valid = false;
  draft.push({ text: "some text", valid: false });
});
```

And finally there are three more interesting libraries for working with immutable structures which may be more familiar for Haskell devs: [partial.lenses](https://github.com/calmm-js/partial.lenses), [monocle-ts](https://github.com/gcanti/monocle-ts), and [optics-ts](https://github.com/akheron/optics-ts).
These are optics libraries.
Unfortunately the first one doesn't have type bindings.
The second one support TypeScript and is a partial porting of Scala monocle.
And the last one is the newest and TypeScript oriented optics library.

### High Order functions and Currying

A lot of languages support High Order functions and TypeScript is not an exception.
You can use functions as an argument and also return them.

```typescript
const filterArray = (fn: (elem: string) => boolean, arr: string[]): string[] => {
  const newArray: string[] = [];
  for (let i = 0; i < arr.length; i++) {
    if (fn(arr[i])) {
      newArray.push(arr[i]);
    }
  }
  return newArray; // or just simple return arr.filter(fn);
};

const f = (x: string): ((s: number) => boolean) => {
  return (s: number) => {
    return s.toString() === x;
  };
};

f("5")(5);
```

As you may see in the last example, we can call function `f` and return function from it, applying in for the new argument.
So, yes, we can implement some king of currying using it.

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

foldArr(add)(0)([1, 2, 3, 4, 5]);
```

Unfortunately syntax is weird as you need to always use named parameters.
But it is still a possible functionality, and you can simply use it in your code.

### Polymorphism

There are different polymorphisms, but we will say only about thee of them: Parametric, Ad hoc, and Row.
Let's start from the first on.
Parametric polymorphism allows us to write abstract function or data which will not depend on its type.
In TypeScript, it is called generics.

```typescript
const arrLength: <T>(_: T[]) => number = (arr) => arr.length;

arrLength<number>([1, 2, 3]);
arrLength<string>(["1", "2", "3"]);


type NonEmpty<T> = {
  head: T;
  tail: T[];
};
```

Ad hoc polymorphism allows you to implement abstract functions, which logic will be different with different types.

```typescript
interface Eq<T> {
  equal: (f: T, s: T) => boolean;
}

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

const lookup = <T, K extends Eq<T>, V>(cmp: K, k: T, mp: [T, V][]): V | undefined => {
  let result: V | undefined;
  mp.forEach(([kk, v]) => {
    if (cmp.equal(k, kk)) {
      result = v;
    }
  });
  return result;
};

lookup(new IntEq(), 1, [[1, "2"], [2, "1"]]);

lookup(new IntArrEq(), [1, 2, 3], [[[1, 3], "1"], [[3, 2], "2"], [[2, 1, 3], "3"]]);
```

One more polymorphism is row.
It allows you to abstract by the record fields and allow only those of them which have such fields.
Unfortunately TypeScript type compatibility is based on structural subtyping, and it is not a row polymorphism.
Despite this we can use intersection types to emulate this.
But you should be careful with using it because subtyping cause information loss as if you have value `a: T` and `b: T & {x: number}` you will be able to do `a = b` and cut off `b` to `T`.

```typescript
type fnT = <T>(v: T & { x: number }) => T & { x: number }
// You should fully describe all types to not lose information.

type A = {};
type B = A & {x: number};

let a: A = {};
const b: B = {x: 1};
a = b;
console.log(a.x); // Property 'x' does not exist on type 'A'.
```

### Mapped types, Conditional types and Type families

One useful type functionality of TypeScript are Mapped and Conditional types.
They give us really flexible ability to modify existing types or create now one.

Mapped types allows you to create new types based on old types transforming them in some way.
The syntax looks like `[set of field names]: type;`.
For example, you can add optional modifier or readonly modifier to each field with `type Partial<T> = { [P in keyof T]?: T[P] }` and `type Readonly<T> = { readonly [P in keyof T]: T[P] }`.
It is also possible to remove such modifiers using `-?` and `-readonly`.
Let's present some examples of mapped types usage.

```typescript
const sym: unique symbol = Symbol();

type Point = {
  x: number;
  y: number;
  [sym]: string;
  0: string;
}

type Pick<T, K extends keyof T> = {
  [P in K]: T[P];
};

type PointValues = Pick<Point, "x" | "y" | 0> // type PointValues = { x: number; y: number; 0: number }

type Getter<T extends string> = `get${Capitalize<T>}`;
type Getters<T> = {
    [K in keyof T as Getter<Extract<K, string>>]: () => T[K];
}

type PointGetters = Getters<Point> // type PointValues = { getX: number; getY: number; }
```

Another great feature is conditional types.
It allows you to choose the type based on some condition expressed as another type.
Using `extends` word you are able to make a condition `T extends U ? X : Y`.
It also allows you to not only choose from two types, but from many types `T extends U ? X : T extends W ? Y : Z`.
With such functionality you may implement things which may resemble type families although not quite right.
Anyway in conjunction with mapped types it is a great feature to manage your types.

```typescript
// never type describe the type of values that never present
type Exclude<T, U> = T extends U ? never : T;

type Omit<T, K> = Pick<T, Exclude<keyof T, K>>;

type Point = {
  x: number;
  y: number;
  z: number;
}

type PointX = Omit<Point, "y" | "z">; // type PointX = { x: number }
```

TypeScript [contains](https://www.typescriptlang.org/docs/handbook/utility-types.html) a lot of predefined conditional and mapped types, so you can use them from the box.

## Going deeper

Build in concepts are good.
But as you already saw TypeScript type system is not really weak.
Adding our knowledge from Haskell and type theory we can think about how to get more from this system and what we can build using it.
This part will show very brief descriptions of different approaches.
But links on references and other articles must help you to deeply understand them.

### HKT

You may see that TypeScript type system is not so bad as one may think.
But it has one important limitation -- absence of kinds.
Higher-Kinded Types allow us to write types which have their own type constructors.
So, using generics instead of abstracting only over types we are able to abstract over types which already abstract over types.

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

Fortunately we are able to simulate kinds using denationalization which allows us to translate higher-order programs into a first-order language.
The main idea is to map type constructors names to their implementations.
Using it we will be able to write type Kind which work with `* -> *` constructors, Kind2 for `* -> * -> *`, and so on.

```typescript
interface URItoKind<A> {
  'Array': Array<A>;
}

interface URItoKind2<A, B> {
  'Map': Map<A, B>;
}

type URIS = keyof URItoKind<unknown>;
type URIS2 = keyof URItoKind2<unknown, unknown>;

type Kind<F extends URIS, A> = URItoKind<A>[F];
type Kind2<F extends URIS2, A, B> = URItoKind2<A, B>[F];
```

And now we can return to `Collection` above and implement it using kinds.

```typescript
interface Collection<F extends URIS> {
  create: <A>() => Kind<F, A>;
  insert: <A>(v: A) => (c: Kind<F, A>) => Kind<F, A>;
}

const collectionArray: Collection<"Array"> = {
  create: () => [],
  insert: (v) => (c) => [...c, v]
};
```

Of course this is a tiny set of what you can do with kinds.
Most of all this part is inspired by [fp-ts](https://gcanti.github.io/fp-ts/) library and [Yuriy Bogomolov](https://ybogomolov.me/) blog.
Fp-ts contains tone of things which you may know from Haskell and in blog you can find great explanations of it.
Also, there are a lot of libraries based on fp-ts: [io-ts](https://github.com/gcanti/io-ts), [parser-ts](https://github.com/gcanti/parser-ts), [monocle-ts](https://github.com/gcanti/monocle-ts), [remote-data-ts](https://github.com/devex-web-frontend/remote-data-ts), etc.

### Peano numbers

Moving deeper we can think about type level programming and more concretely about computational on types.
The basic primitive which may help us in this problem is Peano numbers.
Lets how it may look in TypeScript.

```typescript
type Zero = "zero";

type Nat = Zero | { n: Nat };

type Succ<N extends Nat> = { n: N };
```

With simple `Nat` definition we are able to make smth useful.
Let's try to create type safe vector which will store its length in type.

```typescript
type Nil = "nil";

type Vec<A, N extends Nat> = N extends Succ<infer R> ? Cons<A, R> : Nil;

type Cons<A, N extends Nat> = {a: A, v: Vec<A, N>};

const emptyVec: <A>() => Vec<A, Zero> = () => "nil";

const pushVec: <A, N extends Nat>(a: A, v: Vec<A, N>) => Vec<A, Succ<N>> = (a, v) => {
  return {a, v}
};

let empty: Vec<number, Zero> = emptyVec();
let oneElem: Vec<number, Succ<Zero>> = pushVec(1, empty);
let twoElems: Vec<number, Succ<Succ<Zero>>> = pushVec(2, oneElem);
let twoElemsInvalid: Vec<number, Succ<Zero>> = pushVec(2, oneElem); // error
oneElem = twoElems; // error
```

This was a simple example what we can do with type level programming.
Using such primitives we can implement different operations of such types and even Fibonacci implementation for example.
Follow Mathias Jean Johansen [blog post](https://mjj.io/2021/03/29/type-level-programming-in-typescript/) of mentioned above Yuriy Bogomolov [examples](https://github.com/YBogomolov/talk-typelevel-ts/blob/master/src/typelevel.d.ts).

```typescript
type Fibonacci<N, F0 = Zero, F1 = One> = {
  acc: F0
  n: N extends Succ<infer _> ? Fibonacci<Decrement<N>, F1, Add<F0, F1>> : never
}[IfElse<Equals<Zero, N>, "acc", "n">]
```

### GADT and eDSL

Generalized algebraic data types in Haskell give us an ability to manually write types of the constructors.
Having data type `D a` with type value `a` we are able to create constructors like `C :: Int -> D Int`.
The key feature here is that Haskell may say us about equality of types `a` and `Int` here.

TypeScript on the other hand doesn't have such ability.
Nevertheless, we can manually say for it about such equality passing the proof to it.

The other great feature of this approach is that we can make nice eDSL using GADT.
And again TypeScript will not give you such a good eDSL as a result, but it doesn't mean that we can't do it.

```typescript
import { identity } from "fp-ts/lib/function";

interface Equality<A, B> {
  (a: A): B;
}

type ArithExpr<A> =
  | { type: "Num"; v: number; proof: Equality<number, A> }
  | { type: "Plus"; l: ArithExpr<number>; r: ArithExpr<number>; proof: Equality<number, A> }
  | { type: "Gt"; l: ArithExpr<number>; r: ArithExpr<number>; proof: Equality<boolean, A> }
  | { type: "And"; l: ArithExpr<boolean>; r: ArithExpr<boolean>; proof: Equality<boolean, A> };

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

const wrongExpr = and(num(23), num(12)); // error
```

Another way to implement eDSL is tagless final.
Moving from data type to type class we are able to implement the same logic.
We will use `fp-ts` to implement it.

```typescript
import { Kind, URIS } from "fp-ts/HKT";

type ArithExpr<Expr extends URIS> = {
  num: (v: number) => Kind<Expr, number>;
  plus: (l: Kind<Expr, number>, r: Kind<Expr, number>) => Kind<Expr, number>;
  gt: (l: Kind<Expr, number>, r: Kind<Expr, number>) => Kind<Expr, boolean>;
  and: (l: Kind<Expr, boolean>, r: Kind<Expr, boolean>) => Kind<Expr, boolean>;
};

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

const testExpr: <Expr extends URIS>(E: ArithExpr<Expr>) => Kind<Expr, boolean> = (E) =>
  E.and(E.gt(E.plus(E.num(23), E.num(12)), E.num(170)), E.gt(E.num(35), E.num(47)));

testExpr(arithInterpreter).interpret; // false
testExpr(arithToS).toString; // (((23 + 12) > 170) && (35 > 47))
```

As in the previous parts you can also see more complex examples in Yuriy Bogomolov [edsl workshop](https://github.com/YBogomolov/workshop-edsl-in-typescript/tree/master) and Giulio Canti [example](https://gist.github.com/gcanti/9a0c2a666621f03b80457831ff3ab997).

## Conclusion

This article is not a tutorial of learning typescript.
A lot of things are omitted and other used without any explanation.
But it may help you to understand what things you can do with types in TypeScript.

The last word of this article is by you.
Use things you learned here, develop your own solutions based on them and dive into a great world of frontend development with types.

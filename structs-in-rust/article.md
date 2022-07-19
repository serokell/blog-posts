# Structs in Rust
Almost every programming language allows the creation of some kind of data structure (class, record, data) which can pack a group of things together. Rust is no exclusion with its struct.

By the end of this article you will learn:
- what a struct is
- how to define and initialize a struct
- what is a tuple struct
- traits and traits deriving
- struct methods

Code from this post is available at [Gist](https://gist.github.com/sancho20021/d6faa25ef319db2442cdfbb0aef6b9a0).

## What is a struct in Rust?
Struct is a structure that holds multiple related values. Let's jump straight to our first example and define a Point — wrapper for two coordinates.
```
// [1]   [2]
struct Point {
// [3] [4]
    x: i32,
    y: i32,
}

// We
// [1]: tell Rust that we want to define a new struct
// [2]: named "Point"
// [3]: that contains two fields with names "x" and "y"
// [4]: both of types "i32"
```
Struct in Rust reflects the same idea as a struct in C or a class in Java. Consider the analogue of the above code in mentioned languages:
```
// C
struct Point {
    int x;
    int y;
};

// Java
class Point {
    int x;
    int y;
}
```

### Unit struct
Unit struct is a trivial example that contains zero fields:
```
struct A;
```
Don't be afraid if you see such strange struct definitions. You may wonder what is the purpose of unit structs when they don't contain any information. In reality, they are being used (see [std::fmt::Error](https://doc.rust-lang.org/std/fmt/struct.Error.html), [chrono::offset::Utc](https://docs.rs/chrono/0.4.19/chrono/offset/struct.Utc.html)), as they can have methods and different trait implementations. Both things I just mentioned will be discussed further in the article.

### Tuple struct
Fields of a struct don't always need to be named. A tuple struct is an alternative method of grouping multiple values together. You can define tuple structs like so:
```
//     [1]   [2]  [3]
struct Point(i32, i32);

// [1]: Struct name
// [2]: first component / field of type i32
// [3]: second component of type i32
```
### Struct initialization
We already know how to define a struct. Let's write down previous examples.

```
// Ordinary struct
struct Point {
    x: i32,
    y: i32,
}

// Tuple struct
struct PointTuple(i32, i32);
```

Now let's create some objects of type `Point` and `PointTuple`, i.e. instances of our structs.

```
//             [1]
let p_named = Point {
// [2] [3]
    x: 13,
    y: 37,
};

// [1]: struct name
// [2]: field name
// [3]: field value

//               [1]       [2] [3]
let p_unnamed = PointTuple(13, 37);

// [1]: struct name
// [2], [3]: field values
```

As you can see, the syntax of initialization is pretty straightforward. It is very similar to defining a JSON object, but with keys replaced with field names.

Two things to keep in mind:
- All fields must be assigned with values (there are no default parameters in Rust).
- Order in which you list `key: value` pairs doesn't matter. You can do `let strange_order = Point { y: 37, x: 13 };` if you wish to.

### Field access
#### Get
You can get the desired value using well-known dot notation.

```
let x = p_named.x;
let y = p_named.y;
```

One may wonder, what is the syntax to address a specific field of a tuple struct. Fortunately, Rust has chosen the simplest way possible, by enumerating tuple components from *0* to *n*.

```
let x = p_unnamed.0;
let y = p_unnamed.1;
```
#### Set
It is possible to change the value of a field using the same dot notation, but the **struct instance must be defined as mutable**.
```
let mut p = PointTuple(1, 2);
//  ^^^
p.0 = 1000;
assert_eq!(p.0, 1000);
```
Unlike some other languages, in Rust you cannot mark a specific field of a struct as mutable or immutable:

```
struct Struct {
    mutable: mut i32,
//           ^^^
//           Expected type, found keyword `mut`.
    immutable: bool,
}
```
There are two options:
- Have an immutable binding to the struct. Then you cannot change any of its fields.
- Have a mutable binding, in which case you can set whatever field you choose.

### Convenient field init syntax

Let's define another struct and create its instance.

```
struct Bicycle {
    brand: String,
    kind: String,
    size: u16,
    suspension: bool,
}

let b1 = Bicycle {
    brand: String::from("Brand A"),
    kind: String::from("Mtb"),
    size: 56,
    suspension: true,
};
```

#### Struct Update Syntax

It often happens that you want to copy an instance of a struct and modify some (not all) of its values. Imagine that a different bicycle brand manufactures a model with identical parameters, and we need to create an instance of that model.

Obviously, we can move all values manually:

```
let b2 = Bicycle {
    brand: String::from("Other brand"),
    kind: b1.kind,
    size: b1.size,
    suspension: b1.suspension,
};
```
But this method involves too much code. **Struct update syntax** can help:
```
let b2 = Bicycle {
    brand: String::from("Other brand"),
    ..b1
//  ^^ struct update syntax
};
```
`..b1` tells Rust that we want to initialize the rest of `b2`'s fields with the values from `b1`.

#### Field Init Shorthand
It is possible to omit *field* in `field: value` initialization syntax if the value is a variable (or function argument) with a name that matches the field:
```
fn new_bicycle(brand: String) -> Bicycle {
    let kind = String::from("road");
    Bicycle {
        brand,
//      ^^^ instead of "brand: brand"
        kind,
//      ^^^ instead of "kind: kind"
        size: 54,
        suspension: false,
    }
}
```

<!-- Mention newtype idiom??? -->
## Traits
Okay, let's do something with our structs, for example, print them.
```
let p = Point { x: 0, y: 1 };
println!("{}", p);
//            ^^^
//             Compile error
```

Suddenly, the compiler says `"Point" doesn't implement "std::fmt::Display"` and suggests we use `{:?}` instead of `{}`. Ok, why not:

```
let p = Point { x: 0, y: 1 };
println!("{:?}", p);
//              ^^^
//              Compile error
```
Unfortunately, it did not help, we still get the error message `"Point" doesn't implement "Debug"`. Though, we received a note:
```
note: add `#[derive(Debug)]` to `Point` or manually `impl Debug for Point`
```

The answer is that both `Debug` and `std::fmt::Display` are **Traits**, and we need to implement them for `Point`.

### What a trait is?
A trait is similar to an *interface* in Java or a *class* in Haskell. It defines certain functionality we might expect from some type.

Usually, traits declare a list of methods that can be called on the types that implement this trait.

Here is a list of commonly-used traits from the standard library. You can use
- [`Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html) to format (and print) a value using `{:?}`
- [`Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html) to get a duplicate
- [`Default`](https://doc.rust-lang.org/std/default/trait.Default.html) to create a default instance of type
- [`Eq`](https://doc.rust-lang.org/std/cmp/trait.Eq.html) to compare values for equality

In our example, the `Point` needs an implementation of the `Debug` trait. It is required by the `println!()` macro.

There are two ways to implement a trait.
- Manually write an implementation.
- **Derive** it. The compiler is capable of providing basic implementations for a fixed list of traits via the `#[derive]` macro.

In this article, we will cover only the second option, as it is the shortest way to achieve our needs. Let's add a new line before the `Point` definition.

```
#[derive(Debug, PartialEq, Eq, Default)]
struct Point {
    x: i32,
    y: i32,
}
```

#### Usage of Debug
```
let p = Point { x: 0, y: 1 };
println!("{:?}", p);
```

Now the compiler is not complaining, let's run our program.
```
> cargo run
Point { x: 0, y: 1 }
```
We haven't written anything related to formatting `Point` ourselves, but have already got a decent-looking output. Neat!

#### Usage of Eq

You may have noticed additional `Eq` and `PartialEq` traits. After we derived these traits, we can compare two `Point`s for equality:

```
let a = Point { x: 25, y: 50 };
let b = Point { x: 100, y: 100 };
let c = Point { x: 25, y: 50 };

assert!(a == c);
assert!(a != b);
assert!(b != c);
```

#### Usage of Default
For this example, we need to define another struct.
```
#[derive(Debug, PartialEq, Eq, Default)]
struct DoubleBool(bool, bool);

let point: Point = Default::default();
let double_bool: DoubleBool = Default::default();

assert_eq!(point, Point { x: 0, y: 0 });
assert_eq!(double_bool, DoubleBool(false, false));
```

The most important lines in this snippet are those where we create `point` and `double_bool`. Because both `Point` and `DoubleBool` implement `Default`, we can invoke the `default()` method of the `Default` trait in each case to generate an empty value of type `Point` and `DoubleBool` respectively.

*Exercise: try to remove explicit type annotations from lines 3 and 4 and see what the compiler tells you.*

## Struct methods
Just like in Java or C++, in Rust, you can define **methods** for a particular type. This is done in an `impl` block.

```
impl Point {
    // Associated function
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    // Immutable method
    fn has_same_x(&self, other: &Self) -> bool {
        self.x == other.x
    }

    // Mutable method
    fn shift_right(&mut self, dx: i32) {
        self.x += dx;
    }
}

// Mutable binding
let mut point = Point::new(25, 25);
assert!(!point.has_same_x(&Point::new(30, 25)));

// Calling a method that changes the object state
point.shift_right(3);
assert_eq!(point.x, 28);
```

There are two types of functions related to a particular type.
- Associated functions. They don't require an instance to be called on. Like static functions in Java.
- Methods. They are invoked on an instance of a type.

### Self
You may have noticed that methods have `self` as their first argument. It represents the instance of the struct the method is being called on, similar to how it is done in Python. Some syntax sugar is involved here. Let's desugar it in two steps.

|     |                                              |                                             |
| --- | -------------------------------------------- | ------------------------------------------- |
| 1   | `fn has_same_x(&self, other: &Self)`         | `fn shift_right(&mut self, dx: i32)`        |
| 2   | `fn has_same_x(self: &Self, other: &Self)`   | `fn shift_right(self: &mut Self, dx: i32)`  |
| 3   | `fn has_same_x(self: &Point, other: &Point)` | `fn shift_right(self: &mut Point, dx: i32)` |


`Self` is an alias for the type of the impl block. In our case, it is the `Point`.

Methods can mutate the struct instance they are associated with, but this requires `&mut self` as the first argument. This way they can be called only via mutable binding.

### Why methods instead of functions?
There are at least two reasons.

1. Nice-looking dot notation.
2. Code organization. It is a lot more convenient for the user of your library to see all functionality of the struct inside one impl block instead of tens of functions spread all across the module.

## Methods vs traits
A method can be invoked only on a type it is being associated with. Traits, on the other hand, overcome this limitation, as they are usually meant to be implemented by multiple different types. It allows certain functions to be generalized, not focus on one type, but to require just enough constraints from their arguments to be able to complete the implementation. We have already seen such examples:
- `println!("{:?}", ...)`. It doesn't care what object we want to print as long as it implements the `Debug` trait.
- `assert_eq!(...)`.

<!-- ### Extending functionality
Though it was not mentioned in this post, you can [define your own traits](https://doc.rust-lang.org/book/ch10-02-traits.html), and add an implementation of your trait to **existing types**.

### Operators
Want to add, subtract or compare instances of your struct? Overload operators that you need: `+`, `-`, `<`, etc. Implementing traits from modules [ops](https://doc.rust-lang.org/stable/std/ops/) and [cmp](https://doc.rust-lang.org/stable/std/cmp/) will do the job!

### Trait usage doesn't end here
Traits form a strong concept that supports a wide range of different patterns. This post covers just a tiny bit of basic use cases. -->

## Conclusion
In this article, we learnt the basics of structs in Rust. We explored the ways of defining, initializing, and adding implementation blocks to both structs and tuple structs. We also looked at traits and traits deriving.

Struct is not the only way to create custom types. Rust also has [`enum`](https://doc.rust-lang.org/book/ch06-00-enums.html). We did not cover it in this blog post, but such concepts as methods, traits, and deriving can be applied to enums in a similar way.

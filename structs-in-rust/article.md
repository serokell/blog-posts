# Get Started With Rust: Structs

In almost any programming language, you can create data structures – like classes or records – that pack a group of things together. Rust is no exception to this with structs.

This article will show you how to use structs in Rust.

By the end of this article, you'll know:

- how to define and instantiate a struct;
- how to derive a trait for a struct;
- what are struct methods, and how to use them.

All the code from this post is available [here](https://gist.github.com/sancho20021/d6faa25ef319db2442cdfbb0aef6b9a0).

## What is a struct in Rust?

In Rust, a struct is a custom data type that holds multiple related values.

Let's jump straight into our first example and define `Point` – a wrapper for two coordinates.

```rust
// [1]   [2]
struct Point {
// [3] [4]
    x: i32,
    y: i32,
}

// We
// [1]: tell Rust that we want to define a new struct
// [2]: named "Point"
// [3]: that contains two fields with names "x" and "y",
// [4]: both of type "i32".
```


### Tuple structs

You can use a tuple struct to group multiple values together without naming them.

Tuple structs are defined like this:

```rust
//     [1]   [2]  [3]
struct Point(i32, i32);

// [1]: Struct name.
// [2]: First component / field of type i32.
// [3]: Second component of type i32.
```

### How to create an instance of a struct?

Here are the structs that we defined previously.

```rust
// Ordinary struct
struct Point {
    x: i32,
    y: i32,
}

// Tuple struct
struct PointTuple(i32, i32);
```

We can now create instances of these structs – objects of type `Point` and `PointTuple`.

```rust
//             [1]
let p_named = Point {
// [2] [3]
    x: 13,
    y: 37,
};

// [1]: Struct name.
// [2]: Field name.
// [3]: Field value.

//               [1]       [2] [3]
let p_unnamed = PointTuple(13, 37);

// [1]: Struct name.
// [2], [3]: Field values.
```

As you can see, the initialization syntax is pretty straightforward. It’s like defining a JSON object where keys are replaced by field names.

Two things to keep in mind:

- All fields must have values. There are no default parameters in Rust.
- The order of field-value pairs doesn't matter. You can write `let p_strange_order = Point { y: 37, x: 13 };` if you wish to.

### How to access struct fields?

#### Getting a value

You can get the value of a field by querying it via dot notation.

```rust
let x = p_named.x;
let y = p_named.y;
```

One may wonder: what's the syntax to get the value of a specific field of a tuple struct? Fortunately, Rust has chosen the simplest way possible, by indexing tuple components starting from zero.

```rust
let x = p_unnamed.0;
let y = p_unnamed.1;
```

#### Setting a value

It's possible to change the value of a field using dot notation, but the struct variable **must be defined as mutable**.

```rust
let mut p = PointTuple(1, 2);
//  ^^^
p.0 = 1000;
assert_eq!(p.0, 1000);
```

Unlike some other languages, Rust doesn't allow to mark a specific field of a struct as mutable or immutable.

```rust
struct Struct {
    mutable: mut i32,
//           ^^^
//           Expected type, found keyword `mut`.
    immutable: bool,
}
```

If you have an immutable binding to a struct, you cannot change any of its fields. If you have a mutable binding, you can set whichever field you want.

### Reducing boilerplate

Two features reduce boilerplate code while initializing struct fields:

- struct update syntax
- field init shorthand

To illustrate them, we first need to define a new struct and create an instance of it using the usual syntax.

```rust
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

#### Struct update syntax

It often happens that you want to copy an instance of a struct and modify some (but not all) of its values. Imagine that a different bicycle brand manufactures a model with identical parameters, and we need to create an instance of that model.

We can move all values manually:

```rust
let b2 = Bicycle {
    brand: String::from("Other brand"),
    kind: b1.kind,
    size: b1.size,
    suspension: b1.suspension,
};
```

But this method involves too much code. **Struct update syntax** can help:

```rust
let b2 = Bicycle {
    brand: String::from("Other brand"),
    ..b1
//  ^^ struct update syntax
};
```

`..b1` tells Rust that we want to move the remaining `b2`'s fields from `b1`.

#### Field init shorthand

You can omit the field name in `field: value` initialization syntax if the value is a variable (or function argument) with a name that matches the field.

```rust
fn new_bicycle(brand: String, kind: String) -> Bicycle {
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

## Traits

Okay, let's try to do something with our struct. For example, print it.

```rust
let p = Point { x: 0, y: 1 };
println!("{}", p);
//            ^^^
//             Compile error
```

Suddenly, the compiler says `"Point" doesn't implement "std::fmt::Display"` and suggests that we use `{:?}` instead of `{}`.

Ok, why not?

```rust
let p = Point { x: 0, y: 1 };
println!("{:?}", p);
//              ^^^
//              Compile error
```

Unfortunately, that doesn't help. We still get the error message: `"Point" doesn't implement "Debug"`.

But we also get a helpful note:

```rust
note: add `#[derive(Debug)]` to `Point` or manually `impl Debug for Point`
```

The reason for this error is that both `Debug` and `std::fmt::Display` are **traits**. And we need to implement them for `Point` to print out the struct.

### What is a trait?

A trait is similar to an interface in Java or a typeclass in Haskell. It defines certain functionality that we might expect from a given type.

Usually, traits declare a list of methods that can be called on the types that implement this trait.

Here's a list of commonly-used traits from the standard library:

- [`Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html). Used to format (and print) a value using `{:?}`.
- [`Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html). Used to get a duplicate of a value.
- [`Eq`](https://doc.rust-lang.org/std/cmp/trait.Eq.html). Used to compare values for equality.

In our example, `Point` needs an implementation of the `Debug` trait because it's required by the `println!()` macro.

There are two ways to implement a trait.

- **Derive** the implementation. The compiler is capable of providing basic implementations for a fixed list of traits via the `#[derive]` macro.
- Manually write the implementation.

Let's start with the first option. We need to add `#[derive]` before the definition of `Point` and list the traits we want to derive.

```rust
#[derive(Debug, PartialEq, Eq)]
struct Point {
    x: i32,
    y: i32,
}

let p = Point { x: 0, y: 1 };
println!("{:?}", p);
```

Now the compiler doesn't complain, so let's run our program.

```rust
> cargo run
Point { x: 0, y: 1 }
```

We didn't write anything related to formatting `Point` ourselves, but we already have a decent-looking output. Neat!

#### `Eq` and `PartialEq`

You may have noticed that we also derived the `Eq` and `PartialEq` traits.

Because of that, we can compare two `Point`s for equality:

```rust
let a = Point { x: 25, y: 50 };
let b = Point { x: 100, y: 100 };
let c = Point { x: 25, y: 50 };

assert!(a == c);
assert!(a != b);
assert!(b != c);
```

### Manually implementing traits

Trait deriving has its disadvantages.

- Only a [small number of traits](https://doc.rust-lang.org/book/appendix-03-derivable-traits.html) are derivable. Though, [procedural macros](https://doc.rust-lang.org/stable/book/ch19-06-macros.html) allow for creation of custom `derive` attributes.
- Sometimes the derived implementation doesn't match your needs.

That's why it's possible to implement a trait by yourself.

#### Implementing `Display`

When we tried to print a `Point` instance using `"{}"`, the compiler said that `Point` doesn't implement `std::fmt::Display`. This trait is similar to `std::fmt::Debug` but it has a few differences:

- It must be implemented manually.
- It should format values in a prettier way, without containing any unnecessary information.

You can think of the `Debug` and the `Display` as the formatters for programmers and users, respectively.

Let's add a manual implementation of `Display`. In the brackets, we have to define every function that is declared in the [trait](https://doc.rust-lang.org/std/fmt/trait.Display.html). In our case, there is only one — `fmt`.

```rust
impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

```

Now we can print our `Point` struct both for developers and users.

```
let p = Point::new(0, 1);

assert_eq!(format!("{:?}", p), "Point { x: 0, y: 0 }");
assert_eq!(format!("{}", p), "(0, 0)");
```

## Struct methods

Just like in Java or C++, you can define **methods** that are associated with a particular type. This is done in an `impl` block.

```rust
impl Point {
    // Method that can't modify the struct instance
    fn has_same_x(&self, other: &Self) -> bool {
        self.x == other.x
    }

    // Method that can modify the struct instance
    fn shift_right(&mut self, dx: i32) {
        self.x += dx;
    }
}
```

You can call these methods via dot notation.

```rust
// Mutable binding
let mut point = Point { x: 25, y: 25 };
assert!(!point.has_same_x(&Point { x: 30, y: 25 }));

// Calling a method that changes the object state
point.shift_right(3);
assert_eq!(point.x, 28);
```

Methods can mutate the struct instance they are associated with, but this requires `&mut self` as the first argument. This way, they can be called only via mutable binding.

### Self

You may have noticed that methods have `self` as their first argument. It represents the instance of the struct the method is being called on, similar to how it's done in Python. Some syntax sugar is involved here. Let's desugar it in two steps.

|     |                                              |                                             |
| --- | -------------------------------------------- | ------------------------------------------- |
| 1   | `fn has_same_x(&self, other: &Self)`         | `fn shift_right(&mut self, dx: i32)`        |
| 2   | `fn has_same_x(self: &Self, other: &Self)`   | `fn shift_right(self: &mut Self, dx: i32)`  |
| 3   | `fn has_same_x(self: &Point, other: &Point)` | `fn shift_right(self: &mut Point, dx: i32)` |

Don't confuse `self` with `Self`. The latter is an alias for the type of the `impl` block. In our case, it's `Point`.

### Associated functions

You can also use the `impl` block to define functions that don't take an instance of `Self`, like static functions in Java. They are commonly used for constructing new instances of the type.

```rust
impl Point {
    // Associated function that is not a method
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }
}
```

You can call these kinds of functions by using a double semicolon (`::`) like this:

```rust
let point = Point::new(1, 2);
```

### Why use methods instead of regular functions?

Methods are quite useful for making your code better. Let's look at some examples.

#### Nice-looking dot notation

To call methods of a type, we use dot notation.

Besides beautiful syntax, dot notation provides an additional property — autoreferencing — which means you can write `point.shift_right(3);` instead of `(&mut point).shift_right(3);`.

Functions don't have this advantage, you always have to reference:

```rust
shift_right(&mut point, 3);
```

#### Code organization

All methods are placed inside one or multiple `impl` blocks. This implies two things.

First, you don't have to import methods.

```rust
use my_module::MyStruct;

...
let s = MyStruct::new();
s.do_stuff();
s.do_other_stuff();
```

Without methods, it would look like this:

```rust
use my_module::{MyStruct, do_stuff, do_other_stuff};  // ugly

...
let s = MyStruct::new();
do_stuff(&s);
do_other_stuff(&s);
```

Second, methods are namespaced: you don't have name collisions across types.

```rust
my_rectangle.area()
my_circle.area()
```

Without methods, you would need to write something like this:

```rust
rectangle_area(my_rectangle)
circle_area(my_circle)
```

## Methods vs. traits

Methods and traits are somewhat similar, so beginners can mix them up.

The general rule for when to use which is simple for beginners:

- If you are implementing a common functionality for which a trait exists (converting to string, comparison, etc.), try to implement that trait.
- If you are doing something specific to your application, probably use methods in regular `impl` blocks.

A method can be invoked only on the type it is defined for. Traits, on the other hand, overcome this limitation, as they are usually meant to be implemented by multiple different types.

So traits allow certain functions to be generalized. These functions don't take just one type, but a set of types that is constrained by a trait.

We have already seen such examples:

- `println!("{:?}", ...)` doesn't care what kind of an object we want to print as long as it implements the `Debug` trait.
- `assert_eq!(...)` allows to compare objects of any type as long as they implement `PartialEq`.

## Conclusion

In this article, we covered the basics of structs in Rust. We explored the ways of defining, initializing, and adding implementation blocks to both structs and tuple structs. We also looked at traits and how to implement or derive them.

Structs are not the only way to create custom types. Rust also has [enums](https://doc.rust-lang.org/book/ch06-00-enums.html). While we did not cover them in this blog post, concepts as methods, traits, and deriving can be applied to them in a similar way.

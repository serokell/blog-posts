---
title: Practical Nix Flakes
author: Alexander Bantyev <alexander.bantyev@serokell.io>
date: 2021-04-30
---

## Introduction

Flakes are a new feature in the Nix ecosystem. Flakes replace stateful channels (which cause much confusion among novices) and introduce a more intuitive and consistent CLI interface, making them a perfect opportunity to start using Nix. This post is a quick introduction to Nix itself and the flakes feature specifically. It contains examples and advice on using flakes for a real-life use case: building applications in various languages.

Briefly speaking, Nix is a package manager and a build system. Its most important aspect is allowing to write declarative scripts for reproducible software builds. It also helps to test and deploy software systems while using the functional programming paradigm. There is a vast repository of packages for Nix called [nixpkgs](https://github.com/nixos/nixpkgs), and a GNU/Linux distribution that extends the ideas of Nix to the OS level called [NixOS](https://nixos.org). Nix building instructions are called "derivations" and are written in Nix, the programming language. Derivations can be written for packages or even entire systems. After that, they can then be deterministically "realised" (built) via Nix, the package manager. Derivations can only depend on a pre-defined set of inputs, so they are somewhat reproducible. You can read more about the benefits of Nix in my blog post [What is Nix](https://serokell.io/blog/what-is-nix).

Flakes are self-contained units that have inputs (dependencies) and outputs (packages, deployment instructions, Nix functions for use in other flakes). You can think about them as Rust crates or Go modules but language-independent. Flakes have great reproducibility because they are only allowed to depend on their inputs and they pin the exact versions of said inputs in a lockfile. If you're already familiar with Nix, flakes are to Nix expressions what derivations are to build instructions.

## Getting started with Nix

In order to do anything with flakes, you will first have to get "unstable" Nix up and running on your machine. Don't mind that it is called "unstable": it is not generally dangerous to run on your machine, it simply changes more often than "stable". The easiest way is to use the official installer, which will work on any Linux distro, macOS, or Windows Subsystem for Linux:

```sh
curl -L https://nixos.org/nix/install | sh
```

Follow the instructions until you have Nix working on your machine, and then update to unstable with:

```sh
nix-env -f '<nixpkgs>' -iA nixUnstable
```

And enable experimental features with:

```sh
mkdir -p ~/.config/nix
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

If you are using NixOS or have some trouble with installation, consult the NixOS wiki: <https://nixos.wiki/wiki/flakes#Installing_flakes>

## Getting a feel for flakes

Now that you have a "flaky" Nix installed, it's time to use it!

### `nix shell`

First, let's enter a shell that has GNU Hello from nixpkgs' branch `nixpkgs-unstable` in it:

```sh
nix shell github:nixos/nixpkgs/nixpkgs-unstable#hello
```

Note that this will start the same shell as you are running, but add a directory containing the `hello` executable to your `$PATH`. The shell shouldn't look any different from how it was outside the `nix shell`, so don't panic if it looks like nothing is happening! The executable is not installed anywhere per se, it gets downloaded and unpacked in what you can consider a cache directory.

Now, inside that shell, try running `hello`.

Let's go through what this command does. `nix shell` is a nix subcommand that is used to run a shell with some packages available in `$PATH`. Those packages can be specified as arguments in the "installable" format. Each installable contains two parts: the URL (`github:nixos/nixpkgs/master` in this case) and an "attribute path" (`hello` here).

There are a few URL schemes supported:

-   `github:owner/repo/[revision or branch]` and `gitlab:owner/repo/[revision or branch]` (for public repositories on github.com and gitlab.com; note that that the branch name can not contain slashes)
-   `https://example.com/path/to/tarball.tar.gz` for tarballs
-   `git+https://example.com/path/to/repo.git` and `git+ssh://example.com/path/to/repo.git` for plain git repositories (you can, of course, use this for GitHub and GitLab). You can specify the branch or revision by adding `?ref=<branch name here>`
-   `file:///path/to/directory` or `/path/to/directory` or `./path/to/relative/directory` for a local directory
-   `flake-registry-value` for a value from a flake registry (I won't talk about flake registries in this article).

So, there are some other ways to get the same shell:

```sh
nix shell https://github.com/nixos/nixpkgs/archive/nixpkgs-unstable.tar.gz#hello
nix shell 'git+https://github.com/nixos/nixpkgs?ref=nixpkgs-unstable#hello'
nix shell nixpkgs#hello # nixpkgs is specified in the default registry to be github:nixos/nixpkgs
```

As for the attribute path, for now, just know that it's a period-separated list of Nix "attribute names" that selects a flake output according to some simple logic.

Note that in this case, Nix did not have to build anything since it could just fetch GNU Hello and its dependencies from the binary cache. To achieve this, Nix evaluates a *derivation* from the expression, hashes its contents, and queries all the caches it knows to see if someone has the derivation with this hash cached. Nix uses all the dependencies and all the instructions as the input for this hash! If some binary cache has a version ready, it can be *substituted* (downloaded). Otherwise, Nix will build the derivation by first realising (substituting or building) all the dependencies and then executing the build instructions.

You might be wondering where exactly is the executable installed. Well, try `command -v hello` to see that it is located in a subdirectory of `/nix/store`. In fact, all Nix *derivations* have "store paths" (paths located in `/nix/store`) as inputs and outputs.

### `nix build`

If you just want to build something instead of entering a shell with it, try `nix build`:

```sh
$ nix build nixpkgs#hello
```

This will build Hello (or fetch it from the binary cache if available) and then symlink it to `result` in your current directory. You can then explore `result`, e.g.

```sh
$ ./result/bin/hello
Hello, world!
```

### `nix develop`

Despite the use of binary caches, Nix is a sourcecode-first package manager. This means that it has the ability to provide a build environment for its derivations. So, you can use Nix to manage your build environments for you! To enter a shell with all runtime and buildtime dependencies of GNU Hello, use:

```sh
$ nix develop nixpkgs#hello
```

Inside that shell, you can call `unpackPhase` to place GNU Hello sources in the current directory, then `configurePhase` to run `configure` script with correct arguments and finally `buildPhase` to build.

### `nix profile`

Nix implements stateful "profiles" to allow users to "permanently" install stuff.

For example:

```sh
nix profile install nixpkgs#hello
nix profile list
nix profile update hello
nix profile remove hello
```

If you're already familiar with Nix, this is a replacement for `nix-env`.

### `nix flake`

`nix flake` set of subcommands is used to observe and manipulate flakes themselves rather than their outputs.

#### `nix flake show`

This command takes a flake URI and prints all the outputs of the flake as a nice tree structure, mapping attribute paths to the types of values.

For example:

```
$ nix flake show github:nixos/nixpkgs
github:nixos/nixpkgs/d1183f3dc44b9ee5134fcbcd45555c48aa678e93
├───checks
│   └───x86_64-linux
│       └───tarball: derivation 'nixpkgs-tarball-21.05pre20210407.d1183f3'
├───htmlDocs: unknown
├───legacyPackages
│   ├───aarch64-linux: omitted (use '--legacy' to show)
│   ├───armv6l-linux: omitted (use '--legacy' to show)
│   ├───armv7l-linux: omitted (use '--legacy' to show)
│   ├───i686-linux: omitted (use '--legacy' to show)
│   ├───x86_64-darwin: omitted (use '--legacy' to show)
│   └───x86_64-linux: omitted (use '--legacy' to show)
├───lib: unknown
└───nixosModules
    └───notDetected: NixOS module
```

#### `nix flake clone`

`nix flake clone` will clone the flake source to a local directory, similar to `git clone`.

Let's clone some simple flake and use some other `nix flake` subcommands on it:

```sh
nix flake clone git+https://github.com/balsoft/hello-flake/ -f hello-flake
cd hello-flake
```

#### `nix flake lock` (previously `nix flake update`)

Every time you call a Nix command on some flake in a local directory, Nix will make sure that the contents of `flake.lock` satisfy the `inputs` in `flake.nix`. If you want to do just that, without actually building (or even evaluating) any outputs, use `nix flake lock`.

There are also some arguments for flake input manipulation that can be passed to most Nix commands:

-   `--override-input` takes an input name that you have specified in `inputs` of `flake.nix` and a flake URI to provide as this input; - `--update-input` will take an input name and update that input to the latest version satisfying the flake URI from `flake.nix`.

## Writing your own

Now that you know how to interact with flakes, it's time to write one.

### Nix language refresher

The widely used data type in Nix is an attribute set: a data type for storing key-value pairs. It is similar to a JSON object or a hashmap in many languages. Its syntax is confusingly similar to a list of statements in C-like languages:

```nix
{
  hello = "world";
  foo = "bar";
}
```

Is equivalent to this JSON object:

```json
{
    "hello": "world",
    "foo": "bar"
}
```

`hello` and `bar` are commonly referred to as "attributes" or "attribute names"; `"world"` and `"bar"` are "attribute values".

To get an attribute value from an attribute set, use `.`. For example:

```nix
let
  my_attrset = { foo = "bar"; };
in my_attrset.foo
```

(`let ... in` is a way to create bindings; the syntax inside it is identical to that of an attribute set)

You can also abbreviate your attribute set by setting specific attributes with `.` instead of defining the entire set:

```nix
{
  foo.bar = "baz";
}
```

Is equivalent to

```nix
{
  foo = { bar = "baz"; };
}
```

Other types include strings (`"foo"`), numbers (1, 3.1415), heterogenous lists (`[ 1 2 "foo" ]`) and -- quite importantly -- functions (`x: x + 1`).

Functions support pattern matching on attribute sets. For example, this function:

```nix
{ a, b }: a + b
```

When called with `{ a = 10; b = 20; }` will return 30.

Function application is done in ML style:

```nix
let
  f = { a, b }: a + b;
in f { a = 10; b = 20; }
```

The function itself comes first. Then there is a whitespace-separated list of arguments.

If you want to have a function of multiple arguments, use currying:

```nix
let
  f = a: b: a + b;
in f 10 20
```

In this example, `f 10` evaluates to `b: 10 + b`, and then `f 10 20` evaluates to `30`.

If you want to learn more about Nix, check out the [corresponding manual section](https://nixos.org/manual/nix/stable/#chap-writing-nix-expressions) and [Nix Pills](https://nixos.org/guides/nix-pills/).

### Basic flake structure

The language description you got above is far from complete or formal, but it should help you understand and, more importantly, write some simple Nix expressions and, even more importantly, a flake.

A Nix flake is a directory that contains a `flake.nix` file. That file must contain an attribute set with one required attribute -- `outputs` -- and optionally `description` and `inputs`.

`outputs` is a function that takes an attribute set of inputs (there's always at least one input -- `self` -- which refers to the flake that Nix is currently evaluating; this is possible due to laziness). So, the most trivial flake possible is this:

```nix
{
  outputs = { self }: { };
}
```

This is a flake with no external inputs and no outputs. Not very useful, huh?

Well, we can add an arbitrary output to it and evaluate it with `nix eval` to see that it works:

```nix
{
  outputs = { self }: {
    foo = "bar";
  };
}
```

```sh
$ nix eval .#foo
"bar"
```

Still not very useful, though.

Let's make a flake that does something useful! For that, we will most likely need some inputs:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }: { };
}
```

Still not very useful; we don't have any outputs! However, now there is an external `nixpkgs` input.

While the attribute set that `outputs` returns may contain arbitrary attributes, some standard outputs are understood by various `nix` utilities. For example, there is a `packages` output that contains packages. It works well with the commands described in [Getting a feel for flakes](#getting-a-feel-for-flakes). Let's add it!

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.hello = /* something here */;
  };
}
```

First of all, let's understand why we need `x86_64-linux` here. Flakes promise us *hermetic evaluation*, which means that the outputs of a flake should be the same regardless of the evaluator's environment. One particular property of the evaluating environment that's very relevant in a build system is the *platform* (a combination of architecture and OS). Because of this, all flake outputs that have anything to do with packages must specify the platform explicitly in some way. The standard way is to make the output be an attribute set with names being platforms and values being whatever the output semantically represents, but built specifically for that platform. In the case of `packages`, each per-platform value is an attribute set of packages.

You might now think: well then, how can we just write `nix build nixpkgs#hello` and get a package without explicitly specifying the platform? Well, that's because Nix actually does that for you behind the scenes. For `nix shell`, `nix build`, `nix profile`, and `nix develop` (among some other commands), Nix tries to figure out which output you want by trying multiple in a specific order. Let's say you do `nix build nixpkgs#hello` on an x86~64~ machine running Linux. Then Nix will try:

-   `hello`
-   `packages.x86_64-linux.hello`
-   `legacyPackages.x86_64-linux.hello`

We're already familiar with `packages` output; `legacyPackages` is designed specifically for nixpkgs. The nixpkgs repository is a lot older than flakes, so it is impossible to its arbitrary attribute format into neat `packages`. `legacyPackages` was devised to accomodate the legacy mess. In particular, `legacyPackages` allows per-platform packagesets to be arbitrary attribute sets rather than structured packages.

So, let's reexport `hello` from nixpkgs in our own flake:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
  };
}
```

Now we can build the reexported package:

```sh
$ nix build .#hello
```

Or run it:

```sh
$ nix run .#hello
Hello, world!
```

(By default, `nix run` will execute the binary with the same name as the attribute name of the package.)

Hooray! Now we have a flake that outputs a package we can use or build.

Another thing we can add is a "development" shell containing some utilities that might be useful when working on our flake. In this example, maybe we want to have `hello` and `cowsay` in `$PATH` to print the friendly greeting and then make the cow say it. There is a special output for such development shells, called `devShell`. There is also a function for building such shells in nixpkgs. To prevent writing the unwieldy `nixpkgs.legacyPackages.x86_64-linux` multiple times, let's extract it via a `let ... in` binding:

```nix
{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux.hello;
    in {
      packages.x86_64-linux.hello = pkgs.hello;

      devShell.x86_64-linux =
        pkgs.mkShell { buildInputs = [ self.packages.x86_64-linux.hello pkgs.cowsay ]; };
   };
}
```

Now we can enter the development environment with `nix develop`. If you want to run a shell other than Bash in that environment, you can use `nix shell -c $SHELL`.

```sh
$ nix develop -c $SHELL
$ hello | cowsay
 _______________
< Hello, world! >
 ---------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

Let's examine our flake using `nix flake show`:

```
$ nix flake show
path:/path/to/flake
├───devShell
│    └───x86_64-linux: development environment 'nix-shell'
└───packages
      └───x86_64-linux
            └───hello: package 'hello-2.10'
```

Now that we've written our slightly useful "hello" flake, time to move to practical applications!

### Some tips and tricks

#### direnv

You can use [direnv](https://direnv.net/) with [nix-direnv](https://github.com/nix-community/nix-direnv) to automatically enter `devShell` when you change directory into the project which is packaged in a flake. The `.envrc` file is really simple in that case:

```sh
use flake
```

#### flake-utils

There is a library that helps you extract the boring per-platform attrsets away: [`flake-utils`](https://github.com/numtide/flake-utils). If we use flake-utils in our example flake, we can make it support all the nixpkgs platforms with practically no extra code:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.hello = pkgs.hello;

        devShell = pkgs.mkShell { buildInputs = [ pkgs.hello pkgs.cowsay ]; };
      });
}
```

Note how now there are more platforms in the output of `nix flake show`:

```
path:/path/to/flake
├───devShell
│     ├───aarch64-linux: development environment 'nix-shell'
│     ├───i686-linux: development environment 'nix-shell'
│     ├───x86_64-darwin: development environment 'nix-shell'
│     └───x86_64-linux: development environment 'nix-shell'
└───packages
      ├───aarch64-linux
      │     └───hello: package 'hello-2.10'
      ├───i686-linux
      │     └───hello: package 'hello-2.10'
      ├───x86_64-darwin
      │     └───hello: package 'hello-2.10'
      └───x86_64-linux
             └───hello: package 'hello-2.10'
```

All the platform attributes are inserted automatically by flake-utils.

## Packaging existing applications

The most obvious use-case for flakes is packaging up existing applications to get the benefits of the Nix ecosystem. To facilitate this, I have written a library of templates that you can use with `nix flake init -t`. This section will mostly be about tweaking the templates to build your application correctly.

### Haskell (cabal)

Haskell has a prevalent presence in the Nix ecosystem. Nixpkgs contains a complete mirror of Hackage, and there are multiple tools that facilitate building Haskell applications using Nix.

The most popular way of building Haskell applications with Nix is [`cabal2nix`](https://github.com/NixOS/cabal2nix). It extracts dependency information from your project's cabal file and uses the nixpkgs `haskellPackages` collection to resolve those dependencies.

To add it to your existing Haskell application, do:

```sh
nix flake init -t github:serokell/templates#haskell-cabal2nix
```

It will create a `flake.nix` similar to this (note the `throw <...>` replacement for a name; write the name of your project there).

```nix
{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = throw "put your package name here!";
      in {
        packages.${packageName} = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
```

#### Using the flake

You can build the `defaultPackage` using `nix build`, and do all the other usual things: `nix shell`, `nix develop`, etc.

#### Potential modifications and troubleshooting

Unless you need some of the development tools provided in the `devShell` (such as the language server or ghcid), just remove them.

If you need a different version of GHC, choose it in `haskellPackages` definition, for example, to use GHC 9.0.1, do:

```nix
haskellPackages = pkgs.haskell.packages.ghc901;
```

In case some dependency fails to build, you need to override it in the package definition (the last argument of `callCabal2nix`, to be precise). For example, if your project depends on `gi-gtk-declarative` and it is broken in the current version of nixpkgs because some version constraints don't match, do (substituting the failing package name for `gi-gtk-declarative`):

```nix
# <...>
packages.${packageName} =
  haskellPackages.callCabal2nix packageName self rec {
    gi-gtk-declarative =
      jailbreakUnbreak haskellPackages.gi-gtk-declarative;
  };
# <...>
```

Note: `jailbreakUnbreak` only removes cabal version constraints. If there is an actual breaking change in some dependency, the build will still fail. In that case, there's little you can do but try to fix the actual issue with the package, which requires some understanding of how `overrideAttrs` works, and that's out of scope of this tutorial. Don't be afraid to read the nixpkgs manual or ask for help in any of the Nix communities you can find!

If some dependency fails to build because of failing tests (which may be the case when the tests require network access or a local resource unavailable from the Nix build), you can disable them like this:

```nix
# <...>
packages.${packageName} =
  haskellPackages.callCabal2nix packageName self rec {
    gi-gtk-declarative =
      pkgs.haskell.lib.dontCheck haskellPackages.gi-gtk-declarative;
  };
# <...>
```

#### Other build options

Some other options for building Haskell applications and libraries with Nix:

-   [haskell.nix](https://github.com/input-output-hk/haskell.nix)

### Rust (Cargo)

Rust is also quite popular in the Nix community, although due to the relatively young age the integration isn't as good at this time. There are multiple competing ways to build crates. I prefer the way [crate2nix](https://github.com/kolloch/crate2nix) works: it's very similar in operation to cabal2nix, but it's a bit more complicated due to some Cargo peculiarities.

To get a template that builds a single Cargo crate via crate2nix, run `nix flake init -t github:serokell/templates#rust-crate2nix`. The `flake.nix` is going to be similar to this file:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    crate2nix = {
      url = "github:kolloch/crate2nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, crate2nix, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        crateName = throw "Put your crate name here";

        inherit (import "${crate2nix}/tools.nix" { inherit pkgs; })
          generatedCargoNix;

        project = pkgs.callPackage (generatedCargoNix {
          name = crateName;
          src = ./.;
        }) {
          defaultCrateOverrides = pkgs.defaultCrateOverrides // {
            # Crate dependency overrides go here
          };
        };

      in {
        packages.${crateName} = project.rootCrate.build;

        defaultPackage = self.packages.${system}.${crateName};

        devShell = pkgs.mkShell {
          inputsFrom = builtins.attrValues self.packages.${system};
          buildInputs = [ pkgs.cargo pkgs.rust-analyzer pkgs.clippy ];
        };
      });
}
```

#### Troubleshooting and improvements

Suppose your package requires some "native" (non-Rust) libraries, either itself or transitively via a dependency. In that case, you have to specify them manually to `defaultCrateOverrides`, unless they are already specified in nixpkgs' `defaultCrateOverrides`. For example, if your application called `my-music-player` requires `libpulseaudio`,

```nix
# <...>
defaultCrateOverrides = pkgs.defaultCrateOverrides // {
  my-music-player = _: {
    buildInputs = [ pkgs.libpulseaudio ];
  };
}
#<...>
```

#### Alternatives

-   Manually using `buildRustCrate` from nixpkgs
-   [naersk](https://github.com/nmattia/naersk)

### Python (poetry)

Python ecosystem is somewhat supported in nixpkgs via `python3Packages`, but the support is not as complete as for Haskell. However, if your project is packaged with poetry, you're in luck as there is `poetry2nix` which allows you to build the application easily.

To initialize the template, `nix flake init -t github:serokell/templates#python-poetry2nix`. It looks something like this:

```nix
{
  description = "My Python application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        customOverrides = self: super: {
          # Overrides go here
        };

        app = pkgs.poetry2nix.mkPoetryApplication {
          projectDir = ./.;
          overrides =
            [ pkgs.poetry2nix.defaultPoetryOverrides customOverrides ];
        };

        packageName = throw "put your package name here";
      in {
        packages.${packageName} = app;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ poetry ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
```

#### Troubleshooting

In case some dependency fails to build, you need to fix it by overriding it in `customOverrides`. See <https://github.com/nix-community/poetry2nix/blob/master/overrides.nix> for various example overrides.

## Up next

In an upcoming article, I'll explain how to use Flakes to build and run containers and set up infrastructure deployments. To make sure you hear about it, you can follow Serokell on [Twitter](https://twitter.com/serokell). Stay tuned!

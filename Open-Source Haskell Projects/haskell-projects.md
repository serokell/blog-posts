In discussions about Haskell, sometimes a question is raised – what exactly can you build with the language? 

To answer this question, this article will look at some of the more interesting Haskell open-source projects on GitHub. 

Among them, you will find cool user-facing applications, such as [SimpleX](https://serokell.io/blog/best-haskell-open-source-projects#simplex) and [hledger](https://serokell.io/blog/best-haskell-open-source-projects#hledger). There's also plenty of compilers and linters, as well as useful tools and libraries.

Most of the projects are, at the time of writing, actively maintained.

## ShellCheck

The most starred Haskell project on GitHub is [ShellCheck](https://github.com/koalaman/shellcheck), a static analysis tool for analyzing shell scripts. At the time of writing, the project has 29k stars.

The goal of ShellCheck is to help users write correct shell scripts by pointing out syntax and semantic issues. For some examples of what it can prevent, see their [gallery of bad code](https://github.com/koalaman/shellcheck/blob/master/README.md#user-content-gallery-of-bad-code).


## Hasura 

[Hasura](https://github.com/hasura) is an open-source GraphQL engine that gives you instant access to a GraphQL API for your data. Haskell is the most used language in their repository, followed by languages like TypeScript, JavaScript, Python, and Go.


## hledger

Did you know that you can track your finances with Haskell? 💰

[hledger](https://github.com/simonmichael/hledger) is a tool for plain-text accounting that works on Linux, macOS, and Windows. It’s lightweight, cross-platform, and supports double-entry accounting. 


## SimpleX

![SimpleX logo](/files/zi/zi99cxs6.simplex-chat_(1).png)

[SimpleX](https://github.com/simplex-chat/simplex-chat) is a decentralized messaging and application platform without any personal user identifiers. One of the most interesting things about this project is that the SimpleX Chat application is mostly for mobile users – and so they compile Haskell for iOS and Android!

To learn more about the project, you can read [our interview](https://serokell.io/blog/haskell-in-production-simplex) with Evgeniy Poberezkin, the creator of SimpleX.


## Pandoc

[Pandoc](https://github.com/jgm/pandoc) is a widely-used tool that converts text from one markup format to another. To list just some of the formats that this library supports: commonmark, docx, epub, fb2, gfm, and so on.

In fact, we take advantage of the tool in our daily work to generate beautiful, branded audit report PDFs for our clients. 😌


## IHP 

![IHP logo](/files/ic/icgqb6my.ihp_(1).png)

Tired of classic web framework choices like Rails or Express? Perhaps it’s time to try Haskell. 

[IHP](https://github.com/digitallyinduced/ihp) is a web development framework designed to be usable for non-Haskellers and Haskell beginners. In contrast to other popular Haskell web libraries, it’s rather heavyweight and “batteries included”. 

To learn more about the project, you can read [our interview](https://serokell.io/blog/ihp-interview) with Marc Scholten, the CEO of digitally induced. 


## Hakyll

If you want to create a simple personal website, a static website generator is one of the best choices. 

And Haskell has its own static website generator – [Hakyll](https://github.com/jaspervdj/hakyll). It enables you to write content in the language you prefer, define custom config rules with its DSL, and compile the files to HTML. 

It uses one of the previously listed projects – Pandoc – for Markdown and LaTeX support. 

Fun fact: this very website was once using the tool. So it works. 😉 


## Carnap

[Carnap](https://github.com/Carnap/Carnap) is a formal logic framework for creating and exploring formal languages. It’s so awesome it even comes with a [book](https://carnap.io/book) and a [paper](http://eptcs.web.cse.unsw.edu.au/paper.cgi?ThEdu17.5).

The framework consists of a set of core libraries for defining, parsing, and checking formal languages in Carnap, a web-server application, and a set of browser-based applications for educators written using [GHCJS](https://github.com/ghcjs/ghcjs), the Haskell-to-JavaScript compiler.

It was initially developed at Kansas State University but now is used by universities all around the globe. 


## Haskell Dockerfile Linter

[Haskell Dockerfile Linter](https://github.com/hadolint/hadolint) (or Hadolint) is something like ShellCheck but for Docker files. It goes through your Dockerfiles and makes sure that they follow the best practices. If you want to check out how it works, here’s an [online version](https://hadolint.github.io/hadolint/).

Fun fact: it uses ShellCheck for validating the bash code inside the `RUN` instructions. 


## xmonad 

![xmonad logo](/files/1w/1wuqhcey.wordmark_(1).png)

One application that you perhaps wouldn’t expect Haskell to be good for is [tiling window managers](https://en.wikipedia.org/wiki/Tiling_window_manager). But, it turns out, one of the more popular Haskell tools is exactly that.

If you want to configure your desktop with Haskell, [xmonad](https://github.com/xmonad/xmonad) can help you. It’s one of the most customizable and hackable window managers out there.


## KMonad

Once you have customized your desktop, it’s time to customize your keyboard! 😎

[KMonad](https://github.com/kmonad/kmonad) is an advanced keyboard manager that works on Linux, Windows, and macOS. With it, you can customize any key or key combination on your keyboard, create multiple keyboard layers, and create bindings that respond to you holding a key or tapping it multiple times in succession. It works on virtually all keyboards.


## Duckling 

[Duckling](https://github.com/facebook/duckling) is an open-source project by Facebook for parsing text into structured data. It’s a rule-based parser that can detect values like dates, time, numbers, quantities, durations, and distances in many different languages. You can also extend it with your own custom dimensions – types of values to parse. 

Duckling was [originally written](https://github.com/facebookarchive/duckling_old) in Clojure, but it was later rewritten in Haskell to achieve [greater performance and safety](https://medium.com/wit-ai/open-sourcing-our-new-duckling-47f44b776809). 


## Semantic 

[Semantic](https://github.com/github/semantic) is a Haskell library and command-line tool made by GitHub for parsing, analyzing, and comparing source code across many different languages like JavaScript, Python, Go, and more. 


## Taskell

A common task for learning a language is to build your own to-do application. After some time, a toy project like this is usually dropped. But what if you never stopped developing it?

Well, you might end up with something similar to [Taskell](https://github.com/smallhadroncollider/taskell). It’s a CLI task manager with Vim-style key bindings and integrations with Trello and GitHub projects. While it’s written in Haskell, you don’t really need to know Haskell to use it. 

Underneath, it uses Haskell’s [Brick](https://github.com/jtdaugherty/brick) library, which has also been used to create cool TUI apps like [Matterhorn](https://github.com/matterhorn-chat/matterhorn) and command-line [tetris](https://github.com/SamTay/tetris).

## Wasp

[Wasp](https://github.com/wasp-lang/wasp) is an open-source configuration language for developing full-stack web apps with less code.
You describe high-level features of your web app with Wasp DSL and then write the rest of your logic in existing technologies like React, Node.js, Prisma, ... .
While Wasp users don't have any interaction with Haskell, it is the main language used in the Wasp codebase, primarily for implementing Wasp DSL compiler and Wasp CLI.

## Compilers written in Haskell

Haskell is a perfect tool for building classic programming language compilers. 

Here’s 7 languages that have their compiler written in Haskell:

* [Agda](https://github.com/agda/agda) is a dependently-typed programming language and theorem prover. 
* [PureScript](https://github.com/purescript/purescript) is a functional programming language that’s heavily inspired by Haskell and compiles to JavaScript.
* [Unison](https://github.com/unisonweb/unison) is a futuristic functional programming language with content-addressed code.  
* [Kitten](https://github.com/evincarofautumn/kitten) is a statically-typed concatenative systems programming language.
* [Koka](https://github.com/koka-lang/koka) is a functional programming language with effect types. 
* [Carp](https://github.com/carp-lang/Carp) is a typed lisp for real-time applications. 
* [Elm](https://github.com/elm/compiler) is a functional language that compiles to JavaScript.  


## Have anything to add?

As you can see, Haskell can be used to build all kinds of projects: CLI applications, web applications, parsers and compilers, frameworks and engines. It can even be compiled to mobile and JavaScript if necessary.

If you would like to add a cool project to the list, you can do it by submitting an issue in our [GitHub repo](https://github.com/serokell/blog-posts).

And if you want to hear more from us, be sure to give us a follow on [Twitter](https://twitter.com/serokell) or subscribe to the newsletter via the form below.

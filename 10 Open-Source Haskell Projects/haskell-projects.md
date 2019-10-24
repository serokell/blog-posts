Exploring Haskell open-source projects can teach you a lot about functional programming. They help you to grasp the syntax of the language and see how real programming tasks are solved even if you have zero working experience. That is why we’ve chosen 10 popular programs from GitHub written in Haskell for you to study and enjoy. 

## 10 Haskell open-source projects worth studying

### 1. PostgREST

[PostgREST](https://github.com/PostgREST/postgrest) is a tool for accessing RESTful APIs from any PostgreSQL database. Its advantages are that it’s performant, secure and data integrity-friendly:
* The Warp HTTP server of PostgREST is written in Haskell. It processes each request with a new lightweight thread. To improve speed, the server delegates as much calculation as possible to the database like JSON serialization, data validation and authorization.
* When a user needs to access the data, the authentication happens via JSON tokens. PostgREST delegates authorization based on the role information stated in the database. The server assumes the user’s identity and provides info based on this criteria. 
* PostgREST obliges you to put declarative constraints directly into the database you’re building. This way you’ll prevent the data from being corrupted by any API, including your own server.

### 2. git-annex

![git annex haskell open source](/files/hd/hdbr74zs.1.jpg "Git-annex")

[git-annex](https://git-annex.branchable.com/publicrepos/) enables you to manage the files with Git without actually having to check the file contents into Git. It’s handy when one has to deal with large files (Git might not be able to easily handle these because of memory, time, or disk space limitations ). 
git-annex supports both a command line and a folder synchronizer mode.

### 3. Pandoc

[Pandoc](https://github.com/jgm/pandoc) is a free Haskell library that converts one markup format to another. It also includes a command-line tool. Some of the formats this library supports are commonmark, docx, epub, fb2, gfm and so on. It can also create HTML or PDF (via LaTeX). Pandoc usually tries to keep the structure of the original document, but some elements like tables might not fit into the Pandoc’s document format. 

### 4. Cardano SL

![cardano sl serokell](/files/bt/btrqh2n6.2.jpg "Cardano")

[Cardano Settlement Layer](https://github.com/input-output-hk/cardano-sl) is a cryptographic currency developed by Serokell in collaboration with partners. It implements the Ouroboros PoS protocol. Cardano SL provides an elegant solution for a settlement layer of a blockchain platform that increases efficiency and security. 

### 5. Detexify 

![simple symbol search for latex](/files/bc/bcnokoee.photo_2019-10-23_16-42-34.jpg "Detexify")

[This](https://github.com/kirel/detexify-hs-backend) little program simplifies symbol search for those who work with LaTeX. The user just needs to draw a sign in a special window to get a hint. This saves a lot of time, and if you want to, you can contribute to it through the GitHub open repository.

> **If you’re interested to learn more about the latest trends in programming, read our post about [Tagless Final](https://serokell.io/blog/tagless-final).**

### 6. ShellCheck

[ShellCheck](https://github.com/koalaman/shellcheck) is a tool for static analysis of shell scripts. The program synchronizes with the latest versions on Git. Its purpose is to:
* point out typical beginner's syntax mistakes that provoke cryptic error messages;
* solve problems with semantics;
* reveal subtle caveats and pitfalls that cause the script’s failure.

### 7. Darcs 

![darcs version control haskell](/files/v9/v9a3l5ij.4.jpg "Darcs")

[Darcs](http://darcs.net/Development) is a version control system like Git but written in Haskell. The interface is simpler than Git, which might be appreciated by beginners. 
 
### 8. hledger

[hledger](https://hledger.org/) is a Haskell program for tracking time or money that runs on Unix, Mac or Windows. It’s meant to be a free and efficient alternative to accounting apps such as Quicken or GnuCash. 

**hledger features:**
* **A command-line tool**. One can edit data in a text file, and hledger will automatically form reports in various forms, like graphs and tables. 
* **Zero set-up web app**. It’s possible to review the data in the browser.
* **Open-source Haskell library**. You can customize the app by writing your own hledger-compatible scripts and applications. Use the extensive library if needed. 

### 9. Corrode

![legacy code translator](/files/ol/olhyaeh2.5.jpg "Corrode")

Need to move some legacy code from C to Rust? Corrode, the [automatic C-to-Rust translator](https://github.com/jameysharp/corrode) can help you with that. 

The translator preserves the original properties of the program like behavior, maintainability and ABI compatibility. However, it’s still better to check the output because its quality depends on the purity of input data. The result is usually recognizably structured in the same order. 

### 10. membrain
[membrain](https://kowainik.github.io/posts/membrain#a-bit-of-history) is a Haskell library with a type-safe memory data type. Its purpose is to make programmers’ lives easier when they work with memory units. For example, to simplify the definition of different units without fearing to forget to multiply or divide them, combine different units and safely convert between them.

The project was created by [Kowainik](https://twitter.com/kowainik) - a small company that cares about its open-source contributions.

If you would like to contribute to the list and share your favorite one, drop us a line! Also, if you want to progress as a Haskeller, [check out our post](https://serokell.io/blog/learning-haskell) with valuable resources for Haskell programmers.

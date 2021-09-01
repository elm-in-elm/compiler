<p align="center">
<img src="https://raw.github.com/elm-in-elm/compiler/master/assets/elm-in-elm.svg.png" alt="elm-in-elm logo" width="200" height="200">
</p>


# elm-in-elm
Elm compiler written in Elm!

<a href="https://travis-ci.com/elm-in-elm/compiler" alt="Build Status">
  <img src="https://img.shields.io/travis/com/elm-in-elm/compiler/master.svg" /></a>
<a href="https://discordapp.com/invite/pSqzWRV" alt="Discord">
  <img src="https://img.shields.io/discord/578305644780716039.svg?label=discord" /></a>
<a href="https://github.com/elm-in-elm/compiler/commits/master" alt="Latest commits">
  <img src="https://badgen.net/github/last-commit/elm-in-elm/compiler" /></a>
<a href="https://github.com/elm-in-elm/compiler/pulse" alt="Activity">
  <img src="https://img.shields.io/github/commit-activity/m/elm-in-elm/compiler.svg" /></a>
<a href="https://package.elm-lang.org/packages/elm-in-elm/compiler/latest/" alt="Elm package">
  <img src="https://img.shields.io/elm-package/v/elm-in-elm/compiler.svg" /></a>
<br/>
<a href="https://github.com/elm-in-elm/compiler/issues" alt="Contributions welcome!">
  <img src="https://img.shields.io/badge/contributions-welcome-brightgreen.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/graphs/contributors" alt="Contributors">
  <img src="https://img.shields.io/github/contributors/elm-in-elm/compiler.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/labels/help%20wanted" alt="'help wanted' issues">
  <img src="https://img.shields.io/github/issues/elm-in-elm/compiler/help wanted.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/labels/good%20first%20issue" alt="'good first issue' issues">
  <img src="https://img.shields.io/github/issues/elm-in-elm/compiler/good first issue.svg" /></a>

----

## Goals

1. :book: **compiler as Elm library:** so that we can publish it on https://package.elm-lang.org/ and unlock new kinds of Elm applications (like Elm-evaluating Slack bots, [Klipse](https://github.com/viebel/klipse) integration, stepping debuggers, ...)!
2. :children_crossing: **learning friendly:** so that folks can learn how to write a compiler in Elm (similarly to Richard Feldman's [elm-spa-example](https://github.com/rtfeldman/elm-spa-example)). This means `elm-in-elm` is focused on readability, beauty, approachability, simplicity, great docs and great tests first, and only then completeness and speed.
3. :bulb: **exploration ready:** the first two points enable folks to hack on the compiler (as it's written in Elm, which they know, and the code is new-people-friendly) and answer some questions! (For example, what's the best order of optimizations? How would emitting to JavaScript have to look like to make it extra amenable to Google Closure Compiler's advanced optimizations?)
4. :wrench: **extensible:** again, the first two (three?) points make it easy and invite extending the compiler in various ways, eg. a native binary target, different type inference algorithm, new optimizations, `where` syntax, etc.

**In short, `elm-in-elm` aims to unblock and encourage people to play with compilers and the Elm language itself, explore new frontiers and have fun!**

:tv: For more context and information, you can watch [Martin Janiczek's talk from Elm Europe 2019](https://www.youtube.com/watch?v=62khGXfh8zg&list=PL-cYi7I913S_oRLJEpsVbSTq_OOMSXlPD&index=21) which served as an unveiling of `elm-in-elm` to public. Here are :bar_chart: [the slides.](assets/talk.pdf)

## Non-goals :negative_squared_cross_mark:

1. To dethrone or replace [the official Elm compiler](https://github.com/elm/compiler/) written in Haskell.

This is :negative_squared_cross_mark::negative_squared_cross_mark::negative_squared_cross_mark: **NOT THE REASON** and **NOT THE GOAL** :negative_squared_cross_mark::negative_squared_cross_mark::negative_squared_cross_mark: of `elm-in-elm`. We don't want to and aren't planning to divide the community into multiple Elm derivatives, and will actively try to prevent that. `elm-in-elm` is, for all intents and purposes, a sandbox, a place to try out ideas, an experimentation environment.

## What?

`elm-in-elm` consists of:

* [a compiler implementation](src/)
* [a library](src/Elm/Compiler.elm)
* [a CLI tool](cli/index.js)
* and [a test suite](tests/).

It is written in Elm, and compiles Elm to JavaScript, but lays the foundation to be able to compile to different targets in the future.

>  :warning: **Warning!**  `elm-in-elm` is definitely not ready for usage yet, even though its library is published already. The main blocker is the parsers for expressions not being all implemented yet. See the `parse` column in the table below.


## Contributing

Please yes! :heart: Feel free to look around the [<kbd>help wanted</kbd>](https://github.com/elm-in-elm/compiler/labels/help%20wanted) or [<kbd>good first issue</kbd>](https://github.com/elm-in-elm/compiler/labels/good%20first%20issue) issues, have a look around the codebase for some general nitpicks or refactorings, or hit us up on [Discord](https://discordapp.com/invite/pSqzWRV)!

## Roadmap

<p align="center">
<img src="https://raw.github.com/elm-in-elm/compiler/master/assets/roadmap.png" alt="Roadmap" width="584" height="273">
</p>


|                   | parser tests         | optimize tests       | emit tests           | parse                | desugar            | infer types          | optimize             | emit                 |
| ----------------- | -------------------- | -------------------- | -------------------- | -------------------- | ------------------ | -------------------- | -------------------- | -------------------- |
| integers          | :heavy_check_mark:   | :warning: [[2]](#f2) | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :warning: [[2]](#f2) | :heavy_check_mark:   |
| floats            | :heavy_check_mark:   | :x: [[5]](#f5)       | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :x: [[5]](#f5)       | :heavy_check_mark:   |
| characters        | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| strings           | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| booleans          | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| variables         | :warning:            | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| lists             | :heavy_check_mark:   | :x: [[6]](#f6)       | :warning: [[1]](#f1) | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :x: [[6]](#f6)       | :warning: [[1]](#f1) |
| binary operators  | :warning: [[3]](#3)  | :heavy_check_mark:   | :heavy_check_mark:   | :warning: [[3]](#3)  | :heavy_check_mark: | :x:                  | :warning:            | :warning:            |
| lambdas           | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :warning:            | :heavy_check_mark:   |
| function calls    | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| if...then...else  | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| let..in           | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :warning:            | :heavy_check_mark:   | :warning:            |
| case...of         | :warning: [[4]](#4)  | :x:                  | :x:                  | :warning: [[4]](#4)  | :x:                | :x:                  | :x:                  | :x:                  |
| records           | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| record accessors  | :x:                  | :x:                  | :x:                  | :x:                  | :x:                | :x:                  | :x:                  | :x:                  |
| record updates    | :x:                  | :x:                  | :x:                  | :x:                  | :x:                | :x:                  | :x:                  | :x:                  |
| unit type         | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| tuples, 3-tuples  | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| type annotations  | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   |
| type aliases      | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark:   | :heavy_check_mark: | :warning: [[7]](#f7) | :heavy_check_mark:   | :heavy_check_mark:   |
| custom types      | :heavy_check_mark:   | :x:                  | :x:                  | :heavy_check_mark:   | :x:                | :x:                  | :x:                  | :x:                  |
| custom operators  | :x:                  | :x:                  | :x:                  | :x:                  | :x:                | :x:                  | :x:                  | :x:                  |
| shaders (?)       | :x:                  | :x:                  | :x:                  | :x:                  | :x:                | :x:                  | :x:                  | :x:                  |

1. <span id="f1"></span> Lists' `emit` will have to change a bit: conversion from target platform lists to Elm "custom type" lists is now missing; tracked in [#29](https://github.com/elm-in-elm/compiler/issues/29)
2. <span id="f2"></span> To be optimized with multiplication, subtraction, division, modulo, exponentiation... maybe more?
3. <span id="f3"></span> Multi-line binops (probably) don't work correctly now
4. <span id="f4"></span> Custom type patterns aren't supported yet
5. <span id="f5"></span> To be optimized the same way Ints are
6. <span id="f6"></span> Not implemented; partially tracked in [#29](https://github.com/elm-in-elm/compiler/issues/29)
7. <span id="f7"></span> We're currently doing nothing about the type parameters in the type aliases.


## FAQ

1. - **Q:** Why not use [`stil4m/elm-syntax`](https://github.com/stil4m/elm-syntax/) for the parsers?
   - **A:** We'd *love* to use `elm-syntax` - it would save us so much trouble. But that would not be ideal in some regards:
     - less flexibility wrt. how our types look
     - would be less educational (compare with the *learning resource* goal) - would skip parsers entirely
     - even if we didn't use it as a library but copypasted the parsers code, those are written in the 0.18 [`elm-community/parser-combinators`](https://github.com/elm-community/parser-combinators) style - we'd like, again because of the learning resource goal, to have the parsers written in idiomatic [`elm/parser`](https://github.com/elm/parser) style

     But yeah, there's definitely a little bit of NIH syndrome happening :wink:


## Prerequisites

**The easy way:** if you have Nix installed, run
```
$ nix-shell
```
and you'll drop into a shell that has all the dev dependencies set up and ready!

Alternatively, this is what the project needs.

* **`make`** for the [Makefile](Makefile)
* **NodeJS 10+** for the [CLI tool](src/index.js)
* **`elm`**, **`elm-test`** and **`elm-format`**

## Running the compiler

```
$ make
```

Essentially compiles the compiler (using the official Elm compiler :wink: ) to a `build/elm.js` file and runs it using `node`.

**Very handy** for running the whole compiler pipeline on an example project living in `example-project/`, which the CLI is currently hardcoded to try and compile! In some cases this might be more convenient than writing tests - just add an interesting snippet to `example-project/src/Main.elm`, `Debug.log` what you need in the compiler itself, and `make`!

So absolutely feel free to go bonkers on that `example-project/` - it's there for developer convenience!

## Running the tests

```
$ make test
```
Runs `elm-test` on the test suite (gasp!)

## Formatting code

```
$ make format 
```
Runs elm-format.
Make sure to format code before submitting a pull request!

## Small TODOs

>  This is a brain-dump of some low-level stuff. (High-level stuff should be in the roadmap.) My apologies if it's hard to make sense of this! ~janiczek

#### Project management

- [ ] [@janiczek](https://twitter.com/janiczek/): Share your Firefox bookmarks relevant to `elm-in-elm` (ie. talks about Haskell hierarchical optimizations etc.)
- [ ] Add tests for stages other than parsing and emit into the matrix above

#### General

- [ ] Compare our `Main.compile` with official compiler's `Compile.compile` - is that a better API?
- [ ] Types module: remove, refactor into "module per datastructure" style?
- [ ] Deal with kernel modules
- [ ] Deal with ports
- [ ] Deal with effect modules
- [ ] Deal with typeclasses (number, comparable, ...)
- [ ] Deal with pattern matching
- [ ] Deal with custom binary operators

#### Type inference

- [ ] Try the [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032) and see where that leads
- [ ] Let polymorphism :no_mouth:: `Stage.InferTypes.generateEquations`, the `Typed.Let` case. [This paper](http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf) might have a parable written well enough that we might actually understand type schemes from this. Otherwise, ["Write you a Haskell"](http://dev.stephendiehl.com/fun/006_hindley_milner.html) for the rescue! There is also the [Damas and Milner paper](https://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf) proving the inferred type is the most general one.
- [ ] Extensible records [(link)](http://www.cs.ioc.ee/tfp-icfp-gpce05/tfp-proc/21num.pdf): `D. Leijen, “Extensible records with scoped labels,” in Revised Selected Papersfrom the Sixth Symposium on Trends in Functional Programming, TFP 2005,Tallinn, Estonia, 23-24 September 2005.(M. C. J. D. van Eekelen, ed.), vol. 6 of Trends in Functional Programming, pp. 179–194, Intellect, 2005.`
- [ ] Typecheck across modules, not each module separately. This will probably be clearer after we try and implement the library.
- [ ] Annotate type errors with position in source code (for better error messages)
- [ ] Try to find a better name for "occurs check" and make the error message easier to understand
- [ ] Document the typechecking stages better (ie. at all)
- [ ] Rename types to be able to show nice type variables (ie. the classic `a` instead of `type #0` or something). `Stage.InferTypes.getType`

#### Optimizations

- [ ] Experiment with [Prepack](https://prepack.io/)-like optimization: compute everything you can in the compile-time instead of runtime
- [ ] Implement constant propagation?
- [ ] Implement inlining (maybe it will need some heuristic? Look at how other langs do it?)
- [ ] Implement `(<|)` and `(|>)` fusion (eg. transform both `x |> f` and `f <| x` into `f x`)

#### PrepareForBackend

- [ ] Check that the `Lambda` case of `Stage.PrepareForBackend.findDependencies` works correctly

#### Emit

- [ ] Native binary target (x86_64), possibly through LLVM?
- [ ] WebAssembly?
- [ ] Would this simplify / be a good fit for [Elchemy](https://github.com/wende/elchemy) (Elm -> Elixir)?
- [ ] Would this simplify / be a good fit for [philip2](https://github.com/darklang/philip2) (Elm -> OCaml)?
- [ ] Would it be worth concatenating single-arg lambdas back to multi-arg ones (so that we emit eg. `(a,b) => a+b` instead of `(a) => (b) => a+b`)?
- [ ] How to emit `let`? How does official compiler do it? Seems the dependency graph will have to be computed for its binidng too, similarly to how the path to `main` gets computed for the program itself. `Stage.Emit.emitExpr`, the `Let` case.
- [ ] Do we need to mangle variable names? (ie. do what the official compiler does) Maybe not! Check
- [ ] What's good JS style for Google Closure Compiler's advanced optimizations?
- [ ] What's good JS style for UglifyJS?
- [ ] What's good JS style for modern JS engines?

#### Tests

- [ ] Test `Common.unalias`
- [ ] Test `Stage.Desugar.findModuleOfVar`

## Contributors

<table>
  <tbody>
    <tr>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars3.githubusercontent.com/u/149425">
        </br>
        <a href="https://github.com/Janiczek">Martin Janiczek</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars2.githubusercontent.com/u/5399281">
        </br>
        <a href="https://github.com/rlefevre">Rémi Lefèvre</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars0.githubusercontent.com/u/16308754">
        </br>
        <a href="https://github.com/harrysarson">Harry Sarson</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars0.githubusercontent.com/u/197573">
        </br>
        <a href="https://github.com/Warry">Maxime Dantec</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars0.githubusercontent.com/u/16829510">
        </br>
        <a href="https://github.com/aaronjanse">Aaron Janse</a>
      </td>
    </tr>
    <tr>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars1.githubusercontent.com/u/3983879">
        </br>
        <a href="https://github.com/halfzebra">Eduard Kyvenko</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars3.githubusercontent.com/u/6839698">
        </br>
        <a href="https://github.com/sebsheep">Sébastien Besnier</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars3.githubusercontent.com/u/12603666">
        </br>
        <a href="https://github.com/sgdan">sgdan</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars0.githubusercontent.com/u/15112020">
        </br>
        <a href="https://github.com/pablohirafuji">Pablo Hirafuji</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://raw.github.com/elm-in-elm/compiler/master/assets/user-placeholder.png">
        </br>
        You?
      </td>
    </tr>
  <tbody>
</table>

## License

[BSD-3-Clause](License)

## Attribution

* "person using laptop" icon created by [alvaro_cabrera](https://www.freepik.com/alvaro-cabrera)

<p align="center">
<img src="https://raw.github.com/elm-in-elm/compiler/master/assets/elm-in-elm.svg.png" alt="elm-in-elm logo" width="200" height="200">
</p>


# elm-in-elm
Elm compiler written in Elm!

<a href="https://github.com/elm-in-elm/compiler/blob/master/LICENSE" alt="BSD-3-Clause license">
  <img src="https://img.shields.io/github/license/elm-in-elm/compiler.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/pulse" alt="Activity">
  <img src="https://img.shields.io/github/commit-activity/m/elm-in-elm/compiler.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/graphs/contributors" alt="Contributors">
  <img src="https://img.shields.io/github/contributors/elm-in-elm/compiler.svg" /></a>
<a href="https://discordapp.com/invite/d6kkjg7" alt="Discord">
  <img src="https://img.shields.io/discord/578305644780716039.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/labels/help%20wanted" alt="'help wanted' issues">
  <img src="https://img.shields.io/github/issues/elm-in-elm/compiler/help wanted.svg" /></a>
<a href="https://github.com/elm-in-elm/compiler/labels/good%20first%20issue" alt="'good first issue' issues">
  <img src="https://img.shields.io/github/issues/elm-in-elm/compiler/good first issue.svg" /></a>
  
----

## Why?

1. :book: **compiler as library:** so that we can publish it on https://package.elm-lang.org/ and unlock new kinds of Elm applications!
2. :children_crossing: **learning friendly:** so that folks can learn how to write a compiler in Elm (similarly to Richard Feldman's [elm-spa-example](https://github.com/rtfeldman/elm-spa-example). This means elm-in-elm is focused on readability, beauty, approachability, simplicity and great docs and tests first, completeness and speed second.
3. :bulb: **exploration ready:** the first two points enable folks to hack on the compiler (as it's in Elm, which they know, and the code is new-people-friendly) and answer some questions! (For example, what's the best order of optimizations? How would emitting to JavaScript have to look like to make it extra amenable to Google Closure Compiler's advanced optimizations?)
4. :wrench: **extensible:** again, the first two (three?) points make it easy and invite extending the compiler in various ways, eg. a native binary target, different type inference algorithm, new optimizations, `where` syntax, etc.

Essentially, elm-in-elm should unblock people to play with compilers, explore and have fun!

## False "Why?" :negative_squared_cross_mark:

1. To dethrone or replace [the official Elm compiler](https://github.com/elm/compiler/) written in Haskell.

This is :negative_squared_cross_mark::negative_squared_cross_mark::negative_squared_cross_mark: NOT THE REASON and NOT THE GOAL :negative_squared_cross_mark::negative_squared_cross_mark::negative_squared_cross_mark: of elm-in-elm. We don't want to and aren't planning to divide the community into multiple Elm derivates, and will actively try to prevent that. elm-in-elm is, for all intents and purposes, an experimentation environment.

## What?

`elm-in-elm` consists of:

* [a compiler implementation](src/)
* a library :construction: `TODO` :construction:
* [a CLI tool](src/index.js) :construction: `TODO` :construction:
* and [a test suite](tests/).

It is written in Elm, and compiles Elm to JavaScript, but lays the foundation to be able to compile to different targets in the future.


## Contributing [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/elm-in-elm/compiler/issues)

Oh God please yes! :heart: Feel free to look around the [<kbd>help wanted</kbd>](https://github.com/elm-in-elm/compiler/labels/help%20wanted) or [<kbd>good first issue</kbd>](https://github.com/elm-in-elm/compiler/labels/good%20first%20issue) issues, have a look around the codebase for some general nitpicks or refactorings, or hit us up on [Discord](https://discordapp.com/invite/d6kkjg7)!

## Prerequisites

The tooling around this project requires:

* **`make`** for the [Makefile](Makefile)
* **NodeJS 10+** for the [CLI tool](src/index.js)
* **`yarn`** for installing `elm` and `elm-test` (among other dependencies from [package.json](package.json))

or alternatively a good amount of ingenuinity to do stuff in a different-than-planned way.

## Running the compiler

```
$ make
```
Essentially compiles the compiler (using the official Elm compiler :wink: ) to a `build/elm.js` file and runs it using `node`.

## Running the tests

```
$ make test
```
Runs `elm-test` on the test suite (gasp!) 

## TODO

#### Project management

- [ ] Create issues for all the TODOs in the codebase, tag some with <kbd>help wanted</kbd> and <kbd>good first issue</kbd>
- [ ] [@janiczek](https://twitter.com/janiczek/): Write some more TODOs/issues off the top of your head, and maybe some of the long-term plans
- [ ] Travis integration + shields.io badge about it

#### Library

- [ ] Draft the API of the library
- [ ] Implement the library code (probably in different source-dir from the compiler itself)
- [ ] Publish the library
- [ ] Add a shields.io badge :wink: `/elm-package/v/:user/:packageName.svg`
- [ ] Revise the API after trying it out with some toy project

#### Tooling

- [ ] Nix expression for the dependencies and building this project
- [ ] Make the CLI tool not a joke

#### Parsing

- [ ] Hex integers

#### Type inference

- [ ] Try the [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032) and see where that leads

#### Optimizations

- [ ] [Prepack](https://prepack.io/)-like optimization: compute everything you can in the compile-time instead of runtime

#### Emit

- [ ] Native binary target (x86_64), possibly through LLVM?



## Contributors

<table>
  <tbody>
    <tr>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars3.githubusercontent.com/u/149425?v=3&s=150">
        </br>
        <a href="https://github.com/Janiczek">Martin Janiczek</a>
      </td>
      <td align="center">
        <img width="150" height="150"
        src="https://avatars2.githubusercontent.com/u/5399281?v=3&s=150">
        </br>
        <a href="https://github.com/rlefevre">Rémi Lefèvre</a>
      </td>
    </tr>
  <tbody>
</table>

## License

[BSD-3-Clause](License)

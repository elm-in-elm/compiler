# Snapshot report for `integration-tests/typecheck/let-body-visibility/test.test.js`

The actual snapshot is saved in `test.test.js.snap`.

Generated by [AVA](https://avajs.dev).

## Invocation

    'elm-in-elm -m src/Main.elm'

## Stderr

    `Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.␊
    `

## Stdout

    `---------------------------␊
    -- STARTING THE COMPILER --␊
    ---------------------------␊
    Main.main: Value { expression = Let { bindings = [{ body = Int 2, name = "x" }], body = Plus (Int 1) (Argument "x") }, typeAnnotation = Nothing }␊
    Compilation finished, writing output.␊
    ---------------------------␊
    -- WRITING TO FS ----------␊
    ---------------------------␊
    const Main$main = ((() => {const x = 2; return (1 + x);})());␊
    `

## out.js

    'const Main$main = ((() => {const x = 2; return (1 + x);})());'

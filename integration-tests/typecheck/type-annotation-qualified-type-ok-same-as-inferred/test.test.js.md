# Snapshot report for `integration-tests/typecheck/type-annotation-qualified-type-ok-same-as-inferred/test.test.js`

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
    Main.x: Value { expression = String "abc", typeAnnotation = Just { type_ = UserDefinedType { args = [], name = "Foo", qualifiedness = PossiblyQualified Nothing }, varName = "x" } }␊
    Main.main: Value { expression = Var { name = "x", qualifiedness = PossiblyQualified Nothing }, typeAnnotation = Nothing }␊
    Main.Foo: TypeAlias { definition = String, parameters = [] }␊
    Compilation finished, writing output.␊
    ---------------------------␊
    -- WRITING TO FS ----------␊
    ---------------------------␊
    const Main$x = "abc";␊
    const Main$main = Main$x;␊
    `

## out.js

    `const Main$x = "abc";␊
    const Main$main = Main$x;`

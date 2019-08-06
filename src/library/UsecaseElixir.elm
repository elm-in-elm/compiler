module UsecaseElixir exposing (main)

{-| Usecase: Elixir

In other words, a compiler from Elm to another language.

TODandThe://www.youtube.com/watch?v=c8UT-VfjDMYO

-}

main : String -> Result ElixirError String
main moduleSourceCode =
    moduleSourceCode
        |> Elm.Compiler.parseModule
        |> Random.andThen Elm.Compiler.desugarModule
        |> Random.andThen Elm.Compiler.inferModule
        |> Random.map Elm.Compiler.optimizeModule
        |> Random.andThen Elm.Compiler.......................


FORMAT:

Case

code before

code after (our imaginary API)

------------------------------

Literal literal

( unusedId0
, varIds0
, Typed.Literal literal
)

(fresh (Typed.Literal literal))

------------------------------

Argument name

( unusedId0
, Dict.Any.insert name unusedId0 varIds0
, Typed.Argument name
)

(fresh (Typed.Argument name)
    |> rememberVar name
)

------------------------------

Var name

( unusedId0
, varIds0
, Typed.Var name
)

(fresh (Typed.Var name))

------------------------------

Plus e1 e2

assignIdsHelp unusedId0 varIds0 e1
    |> Result.andThen
        (\( unusedId1, varIds1, e1_ ) ->
            assignIdsHelp unusedId1 varIds1 e2
                |> Result.map
                    (\( unusedId2, varIds2, e2_ ) ->
                        ( unusedId2
                        , varIds2
                        , Typed.Plus e1_ e2_
                        )
                    )
        )

(freshWith2 Typed.Plus
    (recursive e1)
    (recursive e2)
)

------------------------------

Lambda argument body

assignIdsHelp unusedId0 varIds0 body
    |> Result.andThen
        (\( unusedId1, varIds1, body_ ) ->
            Dict.Any.get argument varIds1
                |> Result.fromMaybe (UnknownName argument)
                |> Result.map
                    (\argumentId ->
                        ( unusedId1
                        , varIds1
                        , Typed.lambda argument body_ argumentId
                        )
                    )
        )

(freshWith1AndVar (Typed.lambda argument)
    (recursive body)
    argument
)

------------------------------

Call fn argument

assignIdsHelp unusedId0 varIds0 fn
    |> Result.andThen
        (\( unusedId1, varIds1, fn_ ) ->
            assignIdsHelp unusedId1 varIds1 argument
                |> Result.map
                    (\( unusedId2, varIds2, argument_ ) ->
                        ( unusedId2
                        , varIds2
                        , Typed.Call
                            { fn = fn_
                            , argument = argument_
                            }
                        )
                    )
        )

(freshWith2 Typed.Call
    (recursive fn)
    (recursive argument)
)

------------------------------

If test then_ else_

assignIdsHelp unusedId0 varIds0 test
    |> Result.andThen
        (\( unusedId1, varIds1, test_ ) ->
            assignIdsHelp unusedId1 varIds1 then_
                |> Result.andThen
                    (\( unusedId2, varIds2, then__ ) ->
                        assignIdsHelp unusedId2 varIds2 else_
                            |> Result.map
                                (\( unusedId3, varIds3, else__ ) ->
                                    ( unusedId3
                                    , varIds3
                                    , Typed.If
                                        { test = test_
                                        , then_ = then__
                                        , else_ = else__
                                        }
                                    )
                                )
                    )
        )

(freshWith3 Typed.If
    (recursive test)
    (recursive then_)
    (recursive else_)
)

------------------------------

Let bindings body

assignIdsHelp unusedId0 varIds0 body
    |> Result.andThen
        (\( unusedId1, varIds1, body_ ) ->
            bindings
                |> Dict.Any.foldl
                    (\_ binding accResult ->
                        accResult
                            |> Result.andThen
                                (\( unusedId2, varIds2, newBindings ) ->
                                    assignIdsHelp unusedId2 varIds2 binding.body
                                        |> Result.map
                                            (\( unusedId3, varIds3, newBody ) ->
                                                ( unusedId3
                                                , varIds3
                                                , newBindings
                                                    |> Dict.Any.insert binding.name
                                                        { name = binding.name
                                                        , body = newBody
                                                        }
                                                )
                                            )
                                )
                    )
                    (Ok ( unusedId1, varIds1, Dict.Any.empty Common.varNameToString ))
                |> Result.map
                    (\( unusedId4, varIds4, bindings_ ) ->
                        ( unusedId4
                        , varIds4
                        , Typed.Let
                            { bindings = bindings_
                            , body = body_
                            }
                        )
                    )
        )

(freshWith2 Typed.Let
    (rememberMultiple bindings) -- whatever this is...
    (recursive body)
)

------------------------------

Unit

( unusedId0
, varIds0
, Typed.Unit
)

(fresh Typed.Unit)

module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Browser
import Dict exposing (Dict)
import List.Extra


onn : (a -> comparable) -> List a -> Maybe ( a, a )
onn property list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case List.Extra.find (\item -> property item == property head) tail of
                Just duplicate ->
                    Just ( head, duplicate )

                Nothing ->
                    onn property tail


sort : (a -> comparable) -> List a -> Maybe ( a, a )
sort property list =
    let
        re li =
            case li of
                a :: b :: tail ->
                    if property a == property b then
                        Just ( a, b )

                    else
                        re (b :: tail)

                _ ->
                    Nothing
    in
    list
        |> List.sortBy property
        |> re


dict : (a -> comparable) -> List a -> Maybe ( a, a )
dict property list =
    let
        re di li =
            case li of
                [] ->
                    Nothing

                head :: tail ->
                    let
                        p =
                            property head
                    in
                    case Dict.get p di of
                        Just dup ->
                            Just ( head, dup )

                        Nothing ->
                            re (Dict.insert p head di) tail
    in
    re Dict.empty list


onnsort : (a -> comparable) -> List a -> Maybe ( a, a )
onnsort property list =
    case list of
        a :: b :: c :: d :: e :: f :: g :: _ ->
            sort property list

        _ ->
            onn property list


sortx : (a -> comparable) -> List a -> Maybe ( a, a )
sortx property list =
    let
        re li =
            case li of
                [] ->
                    Nothing

                a :: long ->
                    case long of
                        [] ->
                            Nothing

                        b :: short ->
                            if property a == property b then
                                Just ( a, b )

                            else
                                re long
    in
    list
        |> List.sortBy property
        |> re


suite : Benchmark
suite =
    let
        size =
            100

        target =
            List.range 1 size
    in
    describe (String.fromInt size ++ " items")
        [ benchmark "onn" <|
            \_ -> onn identity target
        , benchmark "sort" <|
            \_ -> sort identity target
        , benchmark "dict" <|
            \_ -> dict identity target
        , benchmark "onnsort" <|
            \_ -> onnsort identity target
        , benchmark "sortx" <|
            \_ -> sortx identity target
        ]


main =
    program suite

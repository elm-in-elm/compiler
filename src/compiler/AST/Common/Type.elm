module AST.Common.Type exposing
    ( State
    , Type(..)
    , TypeArgument(..)
    , emptyState
    , getVarId
    , isParametric
    , niceVarName
    , toString
    )

{-| In its own module because both Error.TypeError and AST.Typed need to see it
-}

import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)
import Dict exposing (Dict)


{-| READ THIS!

When adding a case that recurs on Type, you'll have to add a case to
`InferTypes.Unify.unify`:

    | MyNewType Type Type

will have to get a case:

    (MyNewType m1e1 m1e2, MyNewType m2e1 m2e2) ->
        substitutionMap
            |> unify m1e1 m2e1
            |> Result.andThen (unify m1e2 m2e2)

-}
type Type
    = Var Int
    | Function Type Type
    | Int
    | Float
    | Char
    | String
    | Bool
    | List Type
    | Unit
    | Tuple Type Type
    | Tuple3 Type Type Type
    | {- The actual definitions of type aliases and custom types are elsewhere,
         this is just a "pointer", "var".

         This constructor encompasses both type aliases and custom types:
      -}
      UserDefinedType ( ModuleName, VarName ) (List Type)


type TypeArgument
    = ConcreteType Type
    | TypeParameter String


getVarId : Type -> Maybe Int
getVarId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing


isParametric : Type -> Bool
isParametric type_ =
    case type_ of
        Var _ ->
            True

        Function input output ->
            [ input, output ]
                |> List.any isParametric

        List element ->
            isParametric element

        Tuple left right ->
            [ left, right ]
                |> List.any isParametric

        Tuple3 left middle right ->
            [ left, middle, right ]
                |> List.any isParametric

        _ ->
            False



--------------
-- toString --
--------------


type State
    = State
        { mapping : Dict Int String
        , counter : Int
        }


emptyState : State
emptyState =
    State
        { mapping = Dict.empty
        , counter = 0
        }


toString : State -> Type -> ( String, State )
toString state type_ =
    case type_ of
        Var int ->
            getName state int

        Function t1 t2 ->
            let
                ( t1String, state1 ) =
                    toString state t1
                        |> maybeWrapParens t1

                ( t2String, state2 ) =
                    toString state1 t2
            in
            ( t1String ++ " -> " ++ t2String
            , state2
            )

        List param ->
            let
                ( paramString, state1 ) =
                    toString state param
                        |> maybeWrapParens param
            in
            ( "List " ++ paramString
            , state1
            )

        Int ->
            ( "Int", state )

        Float ->
            ( "Float", state )

        Char ->
            ( "Char", state )

        String ->
            ( "String", state )

        Bool ->
            ( "Bool", state )

        Unit ->
            ( "Unit", state )

        Tuple t1 t2 ->
            let
                ( t1String, state1 ) =
                    toString state t1

                ( t2String, state2 ) =
                    toString state1 t2
            in
            ( "( " ++ t1String ++ ", " ++ t2String ++ " )", state2 )

        Tuple3 t1 t2 t3 ->
            let
                ( t1String, state1 ) =
                    toString state t1

                ( t2String, state2 ) =
                    toString state1 t2

                ( t3String, state3 ) =
                    toString state2 t3
            in
            ( "( " ++ t1String ++ ", " ++ t2String ++ ", " ++ t3String ++ " )", state3 )

        UserDefinedType ( moduleName, varName ) typeParameters ->
            let
                ( paramsString, state1 ) =
                    if List.isEmpty typeParameters then
                        ( "", state )

                    else
                        List.foldl
                            (\param ( strings, state2 ) ->
                                let
                                    ( string, state3 ) =
                                        toString state2 param
                                in
                                ( string :: strings, state3 )
                            )
                            ( [], state )
                            typeParameters
                            |> Tuple.mapFirst (\paramStrings -> " " ++ String.join " " paramStrings)
            in
            ( ModuleName.toString moduleName
                ++ "."
                ++ VarName.toString varName
                ++ paramsString
            , state1
            )


getName : State -> Int -> ( String, State )
getName ((State { counter, mapping }) as state) varId =
    case Dict.get varId mapping of
        Just letter_ ->
            ( letter_, state )

        Nothing ->
            let
                name =
                    niceVarName counter
            in
            ( name
            , State
                { counter = counter + 1
                , mapping = Dict.insert varId name mapping
                }
            )


{-| We're generating the nice variable names using this sequence:

    a, b, ..., z, | a1, b1, ..., z1, | a2, b2, ..., z2, | ...
    0  1       25 | 26  27       51  | 52  53       77  |

-}
niceVarName : Int -> String
niceVarName id =
    if id < 26 then
        letter id

    else
        let
            wraparounds =
                id // 26

            remainder =
                id |> modBy 26
        in
        letter remainder ++ String.fromInt wraparounds


letter : Int -> String
letter int =
    (int + 97 {- `a` -})
        |> Char.fromCode
        |> String.fromChar


{-|

    maybeWrapParens (List Int) ( "List Int", state )

    -->
    ( "(List Int)", state )

    maybeWrapParens Int ( "Int", state )

    -->
    ( "Int", state )

-}
maybeWrapParens : Type -> ( String, a ) -> ( String, a )
maybeWrapParens type_ ( string, state ) =
    let
        wrapParens : String -> String
        wrapParens x =
            "(" ++ x ++ ")"
    in
    if shouldWrapParens type_ then
        ( wrapParens string, state )

    else
        ( string, state )


{-| "Is there a possibility this type would need to be surrounded by parentheses?

Eg. function types: normally no need for parentheses:

    fn : Int -> Bool

but there are usecases that need parentheses:

    task : Task (Int -> Bool) String

-}
shouldWrapParens : Type -> Bool
shouldWrapParens type_ =
    case type_ of
        Var _ ->
            False

        Function _ _ ->
            True

        Int ->
            False

        Float ->
            False

        Char ->
            False

        String ->
            False

        Bool ->
            False

        List _ ->
            True

        Unit ->
            False

        Tuple _ _ ->
            False

        Tuple3 _ _ _ ->
            False

        UserDefinedType _ params ->
            not (List.isEmpty params)

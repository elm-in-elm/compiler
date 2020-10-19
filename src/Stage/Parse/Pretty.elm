module Stage.Parse.Pretty exposing (..)

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend
import Elm.Data.Located as Located
import Elm.Data.Module as ModuleType exposing (ModuleType)
import Elm.Data.Operator as Operator
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as Concrete exposing (ConcreteType)
import Stage.Parse.Contextualize exposing (Block(..))
import Stage.Parse.Token as Token


type Sexpr a
    = Atom a
    | Many (List (Sexpr a))


printWithIndentationOf : Int -> Sexpr String -> String
printWithIndentationOf indent sexpr =
    case sexpr of
        Atom s ->
            s

        Many ls ->
            case ls of
                [] ->
                    "()"

                [ first ] ->
                    "( "
                        ++ printWithIndentationOf (indent + 1) first
                        ++ " )"

                [ Atom first, Atom second ] ->
                    "( " ++ first ++ ", " ++ second ++ " )"

                [ Many [], Atom second ] ->
                    "( (), " ++ second ++ " )"

                [ Atom first, Many [] ] ->
                    "( " ++ first ++ ", () )"

                [ Many [], Many [] ] ->
                    "( (), () )"

                _ ->
                    printManyItems True indent ls ""
                        ++ ")"


printManyItems : Bool -> Int -> List (Sexpr String) -> String -> String
printManyItems isFirst indent many soFar =
    case many of
        first :: rest ->
            printManyItems
                False
                indent
                rest
                (soFar
                    ++ (if isFirst then
                            "( "

                        else
                            ", "
                       )
                    ++ printWithIndentationOf (indent + 1) first
                    ++ "\n"
                    ++ String.repeat (indent * 2) " "
                )

        [] ->
            soFar


listWith : (a -> Sexpr String) -> List a -> Sexpr String
listWith f ls =
    Many (List.map f ls)


dictEntriesWith : (a -> Sexpr String) -> (b -> Sexpr String) -> Dict a b -> Sexpr String
dictEntriesWith keys values d =
    Dict.toList d |> listWith (\( k, v ) -> Many [ keys k, values v ])


pair : a -> Sexpr a -> Sexpr a
pair key value =
    Many [ Atom key, value ]


block : Block -> Sexpr String
block b =
    case b of
        Module record ->
            Many
                [ Atom "Module"
                , Many
                    [ Atom "ty"
                    , moduleType record.ty
                    ]
                ]

        ValueDeclaration record ->
            Many
                [ Atom "ValueDeclaration"
                , Many
                    [ Atom "name"
                    , Atom (Located.unwrap record.name)
                    ]
                , Many
                    [ Atom "args"
                    , listWith
                        (\locatedArg ->
                            Atom (Located.unwrap locatedArg)
                        )
                        record.args
                    ]
                , Many
                    [ Atom "valueExpr__"
                    , expr record.valueExpr__
                    ]
                ]

        TypeAlias record ->
            Many
                [ Atom "ValueDeclaration"
                , Many
                    [ Atom "ty"
                    , Atom record.ty
                    ]
                , Many
                    [ Atom "genericArgs"
                    , Many
                        (record.genericArgs
                            |> List.map Atom
                        )
                    ]
                , Many
                    [ Atom "expr"
                    , typeExpr record.expr
                    ]
                ]

        CustomType record ->
            Debug.todo ""


expr : Frontend.LocatedExpr -> Sexpr String
expr expr_ =
    case Located.unwrap expr_ of
        Frontend.Int int ->
            pair "Int" (Atom (String.fromInt int))

        Frontend.HexInt int ->
            pair "HexInt" (Atom (String.fromInt int))

        Frontend.Float float ->
            pair "Float" (Atom (String.fromFloat float))

        Frontend.Char char ->
            pair "Char" (Atom (String.fromChar char))

        Frontend.String string ->
            pair "String" (Atom string)

        Frontend.Bool bool ->
            pair "Bool"
                (Atom
                    (if bool then
                        "True"

                     else
                        "False"
                    )
                )

        Frontend.Var { qualifiedness, name } ->
            pair "Var"
                (Many
                    [ pair "qualifiedness" (Atom "TODO")
                    , pair "name" (Atom name)
                    ]
                )

        Frontend.Argument varName ->
            pair "Argument" (Atom varName)

        Frontend.Operator operator lhs rhs ->
            pair "Operator"
                (Many
                    [ pair "op" (operator |> Located.unwrap |> Operator.toString |> Atom)
                    , pair "lhs" (expr lhs)
                    , pair "rhs" (expr rhs)
                    ]
                )

        Frontend.Lambda { arguments, body } ->
            pair "Lambda"
                (Many
                    [ pair "arguments" (listWith Atom arguments)
                    , pair "body    " (Atom "TODO")
                    ]
                )

        Frontend.Call record ->
            pair "Call" (Atom "TODO")

        Frontend.If record ->
            pair "If" (Atom "TODO")

        Frontend.Let record ->
            pair "Let" (Atom "TODO")

        Frontend.List locateds ->
            pair "List" (Atom "TODO")

        Frontend.Unit ->
            Atom "Unit"

        Frontend.Tuple first second ->
            pair "Tuple" (Many [ Atom "TODO", Atom "TODO" ])

        Frontend.Tuple3 first second third ->
            pair "Tuple" (Many [ Atom "TODO", Atom "TODO", Atom "TODO" ])

        Frontend.Record bindings ->
            pair "Record" (Atom "TODO")

        Frontend.Case locatedExpr list ->
            Debug.todo ""


typeExpr : ConcreteType PossiblyQualified -> Sexpr String
typeExpr expr_ =
    case expr_ of
        Concrete.TypeVar string ->
            pair "TypeVar" (Atom string)

        Concrete.Function { from, to } ->
            pair "Function"
                (Many
                    [ pair "from" (typeExpr from)
                    , pair "to" (typeExpr to)
                    ]
                )

        Concrete.Int ->
            Atom "Int"

        Concrete.Float ->
            Atom "Float"

        Concrete.Char ->
            Atom "Char"

        Concrete.String ->
            Atom "String"

        Concrete.Bool ->
            Atom "Bool"

        Concrete.List list ->
            pair "List" (typeExpr list)

        Concrete.Unit ->
            Atom "Unit"

        Concrete.Tuple first second ->
            pair "Tuple" (Many [ typeExpr first, typeExpr second ])

        Concrete.Tuple3 first second third ->
            pair "Tuple" (Many [ typeExpr first, typeExpr second, typeExpr third ])

        Concrete.Record dict ->
            pair "Record" (dictEntriesWith Atom typeExpr dict)

        Concrete.UserDefinedType { qualifiedness, name, args } ->
            let
                (PossiblyQualified q) =
                    qualifiedness
            in
            pair "UserDefinedType"
                (Many
                    [ pair "qualifiedness"
                        (pair "PossiblyQualified"
                            (case q of
                                Just moduleName ->
                                    pair "Just" (Atom moduleName)

                                Nothing ->
                                    Atom "Nothing"
                            )
                        )
                    , pair "name" (Atom name)
                    , pair "args" (listWith typeExpr args)
                    ]
                )


moduleType : ModuleType -> Sexpr String
moduleType m =
    case m of
        ModuleType.PlainModule ->
            Atom "PlainModule"

        ModuleType.PortModule ->
            Atom "PortModule"

        ModuleType.EffectModule ->
            Atom "EffectModule"

module Elm.Data.Operator exposing (..)


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiate
    | And
    | Or
    | Equals
    | GreaterThan
    | GreaterThanEquals
    | LessThan
    | LessThanEquals
    | Append
    | Cons


{-| Operator precedence.

Constructors are ordered from highest precedence to lowest. High precedence
operators bind more strongly to operands and thus end up deeper in the AST.

-}
type OperatorPrecedence
    = Composition
    | Exponentiation
    | Multiplication
    | Addition
    | Concatenation
    | Comparison
    | AndPrec
    | OrPrec
    | Pipe


type OperatorAssociativity
    = ConflictsWithOthers
    | ConflictsWithSelf
    | RightToLeft
    | LeftToRight


getPrecedence : Operator -> OperatorPrecedence
getPrecedence op =
    case op of
        Add ->
            Addition

        Subtract ->
            Addition

        Multiply ->
            Multiplication

        Divide ->
            Multiplication

        Exponentiate ->
            Exponentiation

        And ->
            AndPrec

        Or ->
            OrPrec

        Equals ->
            Comparison

        GreaterThan ->
            Comparison

        GreaterThanEquals ->
            Comparison

        LessThan ->
            Comparison

        LessThanEquals ->
            Comparison

        Append ->
            Concatenation

        Cons ->
            Concatenation


getAssociativity : OperatorPrecedence -> OperatorAssociativity
getAssociativity prec =
    case prec of
        Composition ->
            ConflictsWithOthers

        Exponentiation ->
            RightToLeft

        Multiplication ->
            LeftToRight

        Addition ->
            LeftToRight

        Concatenation ->
            RightToLeft

        Comparison ->
            ConflictsWithSelf

        AndPrec ->
            RightToLeft

        OrPrec ->
            RightToLeft

        Pipe ->
            ConflictsWithOthers


toString : Operator -> String
toString op =
    case op of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"

        Exponentiate ->
            "^"

        And ->
            "&&"

        Or ->
            "||"

        Equals ->
            "=="

        GreaterThan ->
            ">"

        GreaterThanEquals ->
            ">="

        LessThan ->
            "<"

        LessThanEquals ->
            "<="

        Cons ->
            "::"

        Append ->
            "++"

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


getOperatorPrecedence : Operator -> OperatorPrecedence
getOperatorPrecedence op =
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


getOperatorAssociativity : OperatorPrecedence -> OperatorAssociativity
getOperatorAssociativity prec =
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

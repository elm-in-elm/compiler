module Stage.Parse exposing (moduleName, parse)

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Compiler.Error exposing (Error(..), LocatedParseErrorType(..), ParseError(..))
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Declaration as Declaration
    exposing
        ( Constructor
        , Declaration
        , DeclarationBody
        )
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Token as Token exposing (T(..), Token)
import Elm.Data.Type as Type
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)
import Stage.Parse.Lib as P exposing (Parser)


{-| This `parse` function is used only by cli/, not by src/ (library).
Would it make sense to use it in src/ too?
-}
parse :
    FilePath
    -> List Token
    -> Result Error (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
parse filePath tokens =
    P.run (module_ filePath) tokens


module_ : FilePath -> Parser (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
module_ filePath =
    P.succeed
        (\( moduleType_, moduleName_, exposing_ ) imports_ declarations_ ->
            { imports = imports_
            , name = moduleName_
            , filePath = filePath
            , declarations =
                declarations_
                    |> List.map
                        (\almostDeclaration ->
                            let
                                declaration_ =
                                    almostDeclaration moduleName_
                            in
                            ( declaration_.name, declaration_ )
                        )
                    |> Dict.fromList
            , type_ = moduleType_
            , exposing_ = exposing_
            }
        )
        |> P.keep moduleDeclaration
        |> P.keep imports
        |> P.keep (P.many declaration)


{-|

    [Module] UpperName (Dot UpperName)* -> String

-}
moduleName : Parser ModuleName
moduleName =
    P.succeed identity
        |> P.skip (P.optional moduleType)
        |> P.keep (qualified (P.tokenString TUpperName))


{-|

    (UpperName Dot)* finalName -> String

-}
qualified : Parser String -> Parser String
qualified finalName =
    P.succeed
        (\xs last ->
            (xs ++ [ last ])
                |> String.join "."
        )
        |> P.keep
            (P.manyWithSeparator
                { item = P.tokenString TUpperName
                , separator = P.token Token.Dot
                }
            )
        |> P.skip (P.token Token.Dot)
        |> P.keep finalName


moduleDeclaration : Parser ( ModuleType, ModuleName, Exposing )
moduleDeclaration =
    P.succeed
        (\moduleType_ moduleName_ exposing_ ->
            ( moduleType_
            , moduleName_
            , exposing_
            )
        )
        |> P.keep moduleType
        |> P.keep moduleName
        |> P.skip (P.token Token.Exposing)
        |> P.keep exposingList


moduleType : Parser ModuleType
moduleType =
    P.oneOf
        [ plainModuleType
        , portModuleType
        , effectModuleType
        ]


plainModuleType : Parser ModuleType
plainModuleType =
    P.succeed PlainModule
        |> P.skip (P.token Token.Module)


portModuleType : Parser ModuleType
portModuleType =
    P.succeed PortModule
        |> P.skip (P.token Token.Port)
        |> P.skip (P.token Token.Module)


effectModuleType : Parser ModuleType
effectModuleType =
    P.succeed EffectModule
        |> P.skip (P.token Token.Effect)
        |> P.skip (P.token Token.Module)


declaration : Parser (ModuleName -> Declaration LocatedExpr TypeAnnotation PossiblyQualified)
declaration =
    P.succeed
        (\( name, body ) module__ ->
            { module_ = module__
            , name = name
            , body = body
            }
        )
        |> P.keep declarationBody


declarationBody : Parser ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
declarationBody =
    P.oneOf
        [ typeAliasDeclaration
        , customTypeDeclaration
        , valueDeclaration
        , portDeclaration
        ]


portDeclaration : Parser ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
portDeclaration =
    P.succeed (\name type__ -> ( name, Declaration.Port type__ ))
        |> P.skip (P.token Token.Port)
        |> P.keep (P.tokenString Token.TLowerName)
        |> P.skip (P.token Token.Colon)
        |> P.keep type_


valueDeclaration : Parser ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
valueDeclaration =
    P.succeed
        (\annotationOrDeclName maybeAnnotation expr_ ->
            case maybeAnnotation of
                Nothing ->
                    ( annotationOrDeclName
                    , Declaration.Value
                        { typeAnnotation = Nothing
                        , expression = expr_
                        }
                    )

                Just ( type__, declName ) ->
                    ( declName
                    , Declaration.Value
                        { typeAnnotation =
                            Just
                                { varName = annotationOrDeclName
                                , type_ = type__
                                }
                        , expression = expr_
                        }
                    )
        )
        |> P.keep (P.tokenString Token.TLowerName)
        |> P.keep
            (P.oneOf
                [ P.succeed (\type__ declarationName -> Just ( type__, declarationName ))
                    |> P.skip (P.token Token.Colon)
                    |> P.keep type_
                    |> P.keep (P.tokenString Token.TLowerName)
                , P.succeed Nothing
                ]
            )
        |> P.skip (P.token Token.Equals)
        |> P.keep expr


customTypeDeclaration : Parser ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
customTypeDeclaration =
    P.succeed
        (\name parameters constructors_ ->
            ( name
            , Declaration.CustomType
                { parameters = parameters
                , constructors = constructors_
                }
            )
        )
        |> P.skip (P.token Token.Type)
        |> P.keep (P.tokenString Token.TUpperName)
        |> P.keep (P.many (P.tokenString Token.TLowerName))
        |> P.skip (P.token Token.Equals)
        |> P.keep constructors


typeAliasDeclaration : Parser ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
typeAliasDeclaration =
    P.succeed
        (\name parameters type__ ->
            ( name
            , Declaration.TypeAlias
                { parameters = parameters
                , definition = type__
                }
            )
        )
        |> P.skip (P.token Token.Type)
        |> P.skip (P.token Token.Alias)
        |> P.keep (P.tokenString Token.TUpperName)
        |> P.keep (P.many (P.tokenString Token.TLowerName))
        |> P.skip (P.token Token.Equals)
        |> P.keep type_


type_ : Parser (ConcreteType PossiblyQualified)
type_ =
    fnType


fnType : Parser (ConcreteType PossiblyQualified)
fnType =
    P.many1WithSeparator
        { item = literalType
        , separator = P.token Token.RightArrow
        }
        |> P.map
            (\( x, xs ) ->
                List.foldl
                    (\from to ->
                        ConcreteType.Function
                            { from = from
                            , to = to
                            }
                    )
                    x
                    xs
            )


literalType : Parser (ConcreteType PossiblyQualified)
literalType =
    P.oneOf
        [ P.tokenString Token.TLowerName
            |> P.map ConcreteType.TypeVar
        , qualified (P.tokenString Token.TUpperName)
            |> P.map (\upperName -> Debug.todo "UserDefinedType")
        , parenthesizedType
        , recordType
        ]


parenthesizedType : Parser (ConcreteType PossiblyQualified)
parenthesizedType =
    -- TODO tuple types? patternTuple seems pretty nice.
    P.succeed identity
        |> P.skip (P.token Token.LeftParen)
        |> P.keep (P.lazy (\() -> type_))
        |> P.skip (P.token Token.RightParen)


recordType : Parser (ConcreteType PossiblyQualified)
recordType =
    P.sequence
        { start = P.token Token.LeftCurlyBracket
        , separator = P.token Token.Comma
        , end = P.token Token.RightCurlyBracket
        , item = typeBinding
        }
        |> P.map (Dict.fromList >> ConcreteType.Record)


typeBinding : Parser ( VarName, ConcreteType PossiblyQualified )
typeBinding =
    P.succeed Tuple.pair
        |> P.keep (P.tokenString Token.TLowerName)
        |> P.skip (P.token Token.Colon)
        |> P.keep (P.lazy (\() -> type_))


imports : Parser (Dict ModuleName Import)
imports =
    P.many import_
        |> P.map (List.map (\dep -> ( dep.moduleName, dep )) >> Dict.fromList)


exposingList : Parser Exposing
exposingList =
    P.oneOf
        [ exposingAll
        , exposingSome
        ]


exposingAll : Parser Exposing
exposingAll =
    P.succeed ExposingAll
        |> P.skip (P.token Token.All)


exposingSome : Parser Exposing
exposingSome =
    P.succeed ExposingSome
        |> P.keep
            (P.sequence1
                { start = P.token Token.LeftParen
                , separator = P.token Token.Comma
                , end = P.token Token.RightParen
                , item = exposedItem
                }
            )


exposedItem : Parser ExposedItem
exposedItem =
    P.oneOf
        [ exposedValue
        , exposedTypeAndOptionallyAllConstructors
        ]


exposedValue : Parser ExposedItem
exposedValue =
    P.tokenString Token.TLowerName
        |> P.map ExposedValue


exposedTypeAndOptionallyAllConstructors : Parser ExposedItem
exposedTypeAndOptionallyAllConstructors =
    P.succeed
        (\name hasDoublePeriod ->
            if hasDoublePeriod then
                ExposedTypeAndAllConstructors name

            else
                ExposedType name
        )
        |> P.keep (P.tokenString Token.TUpperName)
        |> P.keep
            (P.oneOf
                [ P.succeed True
                    |> P.skip (P.token Token.All)
                , P.succeed False
                ]
            )


expr : Parser LocatedExpr
expr =
    -- TODO how to get from expr to simpleExpr
    P.oneOf
        [ call
        , binop
        ]


simpleExpr : Parser LocatedExpr
simpleExpr =
    P.oneOf
        [ located if_
        , located let_
        , located lambda
        , located literal
        , varOrConstructorValue
        , located list
        , parenthesizedExpr
        , located record
        , located case_
        ]


call : LocatedExpr -> Parser LocatedExpr
call left =
    P.succeed
        (\right ->
            Located.merge
                (\fn argument ->
                    Frontend.Call
                        { fn = fn
                        , argument = argument
                        }
                )
                left
                right
        )
        |> P.keep (P.lazy (\() -> expr))


binop : LocatedExpr -> Parser LocatedExpr
binop left =
    P.succeed (\op right -> Located.merge (BinOp op) left right)
        |> P.keep (P.tokenString Token.TOperator)
        |> P.keep (P.lazy (\() -> expr))


pattern : Parser LocatedPattern
pattern =
    -- TODO how to get from pattern to simplePattern
    P.oneOf
        [ consPatternOperator
        , asAlias
        ]


simplePattern : Parser LocatedPattern
simplePattern =
    P.oneOf
        [ located patternLiteral
        , located patternList
        , patternTuple
        ]


asAlias : LocatedPattern -> Parser LocatedPattern
asAlias left =
    P.succeed (\right -> Located.merge (\pattern_ alias_ -> PAlias pattern_ (Located.unwrap alias_)) left right)
        |> P.skip (P.token Token.As)
        |> P.keep (located (P.tokenString Token.TLowerName))


consPatternOperator : LocatedPattern -> Parser LocatedPattern
consPatternOperator left =
    P.succeed (\right -> Located.merge PCons left right)
        |> P.skip (P.token (Token.Operator "::"))
        |> P.keep pattern


patternLiteral : Parser Pattern
patternLiteral =
    P.oneOf
        [ P.succeed PAnything |> P.skip (P.token Token.Underscore)
        , P.succeed PUnit
            |> P.skip (P.token Token.LeftParen)
            |> P.skip (P.token Token.RightParen)
        , P.tokenChar Token.TChar |> P.map PChar
        , P.tokenString Token.TString |> P.map PString
        , P.tokenInt Token.TInt |> P.map PInt
        , P.tokenFloat Token.TFloat |> P.map PFloat
        , P.tokenString Token.TLowerName |> P.map PVar
        , patternRecord
        ]


patternTuple : Parser LocatedPattern
patternTuple =
    P.sequence1
        { start = P.token Token.LeftParen
        , separator = P.token Token.Comma
        , end = P.token Token.RightParen
        , item = pattern
        }
        |> located
        |> P.andThen
            (\locatedPattern ->
                case List.NonEmpty.toList (Located.unwrap locatedPattern) of
                    [ pattern1, pattern2, pattern3 ] ->
                        P.succeed <|
                            Located.map (\_ -> PTuple3 pattern1 pattern2 pattern3)
                                locatedPattern

                    [ pattern1, pattern2 ] ->
                        P.succeed <|
                            Located.map (\_ -> PTuple pattern1 pattern2)
                                locatedPattern

                    [ pattern__ ] ->
                        P.succeed pattern__

                    _ ->
                        P.fail ExpectedMaxThreeTuple
            )


patternList : Parser Pattern
patternList =
    P.sequence
        { start = P.token Token.LeftSquareBracket
        , separator = P.token Token.Comma
        , end = P.token Token.RightSquareBracket
        , item = pattern
        }
        |> P.map PList


patternRecord : Parser Pattern
patternRecord =
    P.sequence
        { start = P.token Token.LeftCurlyBracket
        , separator = P.token Token.Comma
        , end = P.token Token.RightCurlyBracket
        , item = P.tokenString Token.TLowerName
        }
        |> P.map PRecord


case_ : Parser Expr
case_ =
    P.succeed Frontend.Case
        |> P.skip (P.token Token.Case)
        |> P.keep (P.lazy (\() -> expr))
        |> P.skip (P.token Token.Of)
        |> P.keep (P.many1 caseBranch)


caseBranch : Parser { pattern : LocatedPattern, body : LocatedExpr }
caseBranch =
    P.succeed
        (\pattern_ body ->
            { pattern = pattern_
            , body = body
            }
        )
        |> P.keep pattern
        |> P.skip (P.token Token.RightArrow)
        |> P.keep (P.lazy (\() -> expr))


record : Parser Expr
record =
    P.sequence
        { start = P.token Token.LeftCurlyBracket
        , separator = P.token Token.Comma
        , end = P.token Token.RightCurlyBracket
        , item = recordBinding
        }
        |> P.map Frontend.Record


recordBinding : Parser (Binding LocatedExpr)
recordBinding =
    P.succeed Binding
        |> P.keep (P.tokenString Token.TLowerName)
        |> P.skip (P.token Token.Equals)
        |> P.keep (P.lazy (\() -> expr))


parenthesizedExpr : Parser LocatedExpr
parenthesizedExpr =
    -- TODO tuples from parenStartingExpr. patternTuple seems pretty nice.
    P.succeed identity
        |> P.skip (P.token Token.LeftParen)
        |> P.keep (P.lazy (\() -> expr))
        |> P.skip (P.token Token.RightParen)


list : Parser Expr
list =
    P.sequence
        { start = P.token Token.LeftSquareBracket
        , separator = P.token Token.Comma
        , end = P.token Token.RightSquareBracket
        , item = P.lazy (\() -> expr)
        }
        |> P.map Frontend.List


varOrConstructorValue : Parser LocatedExpr
varOrConstructorValue =
    {- TODO deal with this
       type NameType
           = UppercaseName
           | LowercaseName


       let
           -- unqualified constructor OR qualified var OR qualified constructor
           go : ( List String, NameType, String ) -> Parser Expr
           go ( modules, nameType, lastName ) =
               P.oneOf
                   [ --
                     P.succeed (\( newType, newName ) -> ( lastName :: modules, newType, newName ))
                       |> P.skip (P.token Token.Dot)
                       |> P.keep
                           (P.oneOf
                               [ P.tokenString Token.TUpperName |> P.map (Tuple.pair UppercaseName)
                               , P.tokenString Token.TLowerName |> P.map (Tuple.pair LowercaseName)
                               ]
                           )
                       |> P.andThen go
                   , let
                       toExpr =
                           case nameType of
                               LowercaseName ->
                                   Frontend.Var

                               UppercaseName ->
                                   Frontend.ConstructorValue
                     in
                     P.succeed <|
                       toExpr
                           { qualifiedness = qualify <| List.reverse modules
                           , name = lastName
                           }
                   ]
       in
       P.oneOf
           [ nonqualifiedVar
           , P.tokenString Token.TUpperName
               |> P.andThen (\firstUppercase -> go ( [], UppercaseName, firstUppercase ))
           ]
    -}
    P.oneOf
        [ Debug.todo "unqualified constructor"
        , Debug.todo "qualified var"
        , Debug.todo "qualified constructor"

        -- TODO it's not clear to me after months outside this code: why not unqualified var?
        ]


literal : Parser Expr
literal =
    P.oneOf
        [ P.tokenString Token.TString |> P.map String
        , P.tokenInt Token.TInt |> P.map Int
        , P.tokenFloat Token.TFloat |> P.map Float
        , P.tokenChar Token.TChar |> P.map Char
        ]


lambda : Parser Expr
lambda =
    P.succeed
        (\arguments body ->
            Frontend.Lambda
                { arguments = arguments
                , body =
                    {- Run the promoting transformation on every subexpression,
                       so that after parsing all the arguments aren't unqualified
                       Vars but Arguments.

                       Ie. the lambda parser can't return:

                           -- \x -> x
                           Lambda { argument = VarName "x", body = Var (Nothing, VarName "x") }

                       And instead has to return:

                           -- \x -> x
                           Lambda { argument = VarName "x", body = Argument (VarName "x") }

                       TODO add a fuzz test for this invariant?
                    -}
                    body
                        |> Located.map
                            (Frontend.transform
                                (promoteArguments (List.NonEmpty.toList arguments))
                            )
                }
        )
        |> P.skip (P.token Token.Backslash)
        |> P.keep (P.many1 (P.tokenString Token.TLowerName))
        |> P.skip (P.token Token.RightArrow)
        |> P.keep (P.lazy (\() -> expr))


let_ : Parser Expr
let_ =
    P.succeed
        (\bindings body ->
            let
                bindingNames =
                    List.map .name (List.NonEmpty.toList bindings)
            in
            Frontend.Let
                { bindings =
                    bindings
                        |> List.NonEmpty.map
                            (Binding.map
                                (Located.map
                                    (Frontend.transform
                                        (promoteArguments bindingNames)
                                    )
                                )
                            )
                , body =
                    body
                        |> Located.map
                            (Frontend.transform
                                (promoteArguments bindingNames)
                            )
                }
        )
        |> P.skip (P.token Token.Let)
        |> P.keep (P.many1 letBinding)
        |> P.skip (P.token Token.In)
        |> P.keep (P.lazy (\() -> expr))


letBinding : Parser (Binding LocatedExpr)
letBinding =
    P.succeed Binding
        |> P.keep (P.tokenString Token.TLowerName)
        |> P.skip (P.token Token.Equals)
        |> P.keep (P.lazy (\() -> expr))


if_ : Parser Expr
if_ =
    P.succeed
        (\test then_ else_ ->
            Frontend.If
                { test = test
                , then_ = then_
                , else_ = else_
                }
        )
        |> P.skip (P.token Token.If)
        |> P.keep (P.lazy (\() -> expr))
        |> P.skip (P.token Token.Then)
        |> P.keep (P.lazy (\() -> expr))
        |> P.skip (P.token Token.Else)
        |> P.keep (P.lazy (\() -> expr))


constructors : Parser (NonEmpty (Constructor PossiblyQualified))
constructors =
    P.many1WithSeparator
        { item = constructor
        , separator = P.token Token.Pipe
        }


constructor : Parser (Constructor PossiblyQualified)
constructor =
    P.succeed Declaration.Constructor
        |> P.keep (P.tokenString Token.TUpperName)
        |> P.keep (P.many type_)


import_ : Parser Import
import_ =
    P.succeed
        (\moduleName_ as_ exposing_ ->
            { moduleName = moduleName_
            , as_ = as_
            , exposing_ = exposing_
            }
        )
        |> P.skip (P.token Token.Import)
        |> P.keep moduleName
        |> P.keep
            (P.optional
                (P.succeed identity
                    |> P.skip (P.token Token.As)
                    |> P.keep (P.tokenString Token.TUpperName)
                )
            )
        |> P.skip
            (P.optional
                (P.token Token.Dot
                    |> P.skip (P.fail ExpectedModuleNameWithoutDots)
                )
            )
        |> P.keep
            (P.optional
                (P.succeed identity
                    |> P.skip (P.token Token.Exposing)
                    |> P.keep exposingList
                )
            )


promoteArguments : List VarName -> Expr -> Expr
promoteArguments arguments expr_ =
    -- TODO set of arguments instead of list?
    case expr_ of
        Var var_ ->
            if
                (var_.qualifiedness == PossiblyQualified Nothing)
                    && List.member var_.name arguments
            then
                Argument var_.name

            else
                expr_

        _ ->
            expr_


qualify : List ModuleName -> PossiblyQualified
qualify modules =
    PossiblyQualified <|
        if List.isEmpty modules then
            Nothing

        else
            Just <| String.join "." modules


located : Parser a -> Parser (Located a)
located p =
    P.succeed
        (\( startRow, startCol ) value ( endRow, endCol ) ->
            Located.located
                { start = { row = startRow, col = startCol }
                , end = { row = endRow, col = endCol }
                }
                value
        )
        |> P.keep P.getPosition
        |> P.keep p
        |> P.keep P.getPosition

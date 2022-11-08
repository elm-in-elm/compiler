module Stage.Parse exposing (moduleName, parse)

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Compiler.Error exposing (Error(..), LocatedParseErrorType(..), ParseError(..))
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
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.Token as Token exposing (T(..), Token)
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
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
    P.succeed (List.NonEmpty.toList >> String.join ".")
        |> P.skip (P.optional moduleType)
        |> P.keep
            (P.many1WithSeparator
                { item = P.tokenString TUpperName
                , separator = P.token Token.Dot
                }
            )


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
    {-
       PP.expression
           { oneOf =
               [ PP.literal varType
               , simpleType "Int" ConcreteType.Int
               , simpleType "Float" ConcreteType.Float
               , simpleType "Char" ConcreteType.Char
               , simpleType "String" ConcreteType.String
               , simpleType "Bool" ConcreteType.Bool
               , parenStartingType
               , listType
               , recordType
               , userDefinedType
               ]
           , andThenOneOf =
               [ functionType
               ]
           , spaces = ignorables
           }
    -}
    Debug.todo "type_"


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
    {-
       PP.expression
           { oneOf =
               [ if_
               , let_
               , lambda
               , PP.literal literal
               , always varOrConstructorValue
               , list
               , parenStartingExpr
               , record
               , case_
               ]
           , andThenOneOf =
               -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
               [ PP.infixLeft 99
                   (ignorablesAndCheckIndent (<) ExpectingIndentation)
                   (Located.merge
                       (\fn argument ->
                           Frontend.Call
                               { fn = fn
                               , argument = argument
                               }
                       )
                   )
               , PP.infixLeft 1
                   (checkIndent (<) ExpectingIndentation
                       |> P.andThen (\() -> P.symbol (P.Token "++" ExpectingConcatOperator))
                   )
                   (Located.merge ListConcat)
               , PP.infixLeft 1
                   (checkIndent (<) ExpectingIndentation
                       |> P.andThen (\() -> P.symbol (P.Token "+" ExpectingPlusOperator))
                   )
                   (Located.merge Plus)
               , PP.infixRight 1
                   (checkIndent (<) ExpectingIndentation
                       |> P.andThen (\() -> P.symbol (P.Token "::" ExpectingConsOperator))
                   )
                   (Located.merge Cons)
               ]
           , spaces = ignorables
           }
           |> P.inContext InExpr
    -}
    Debug.todo "expr"


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

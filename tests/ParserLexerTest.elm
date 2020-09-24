module ParserTest2 exposing (test))

import Dict
import Elm.AST.Frontend as Frontend
import Elm.AST.Frontend.Unwrapped exposing (Expr(..), Pattern(..))
import Elm.Compiler.Error exposing (ParseContext, ParseProblem)
import Elm.Data.Declaration as Declaration exposing (DeclarationBody)
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.Module exposing (ModuleType(..))
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Expect exposing (Expectation)
import OurExtras.String as String
import Parser.Advanced as P
import Stage.Parse.Parser
import String.Extra as String
import Test exposing (Test, describe, test)



-- DO NOT EDIT BELOW THIS LINE

inputs : List (String, List (Located LexItem), )

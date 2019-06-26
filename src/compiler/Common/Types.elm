module Common.Types exposing
    ( Binding
    , Dependency
    , Dict_
    , EffectMetadata
    , ExposedItem(..)
    , Exposing(..)
    , FileContents(..)
    , FilePath(..)
    , Module
    , ModuleName(..)
    , ModuleType(..)
    , Modules
    , Project
    , ProjectToEmit
    , Set_
    , TopLevelDeclaration
    , VarName(..)
    )

import Dict.Any exposing (AnyDict)
import Elm.Project
import Set.Any exposing (AnySet)


type alias Set_ a =
    AnySet String a


type alias Dict_ a b =
    AnyDict String a b


type FilePath
    = FilePath String


type ModuleName
    = ModuleName String


type FileContents
    = FileContents String


{-| Each AST stage has its own project fields - that's what the `r` parameter is.
Eg. on the frontend we have `program : Modules Frontend.Expr`
and on the backend we have `graph : Backend.Graph`.
-}
type alias Project r =
    { r
        | elmJson : Elm.Project.Project
        , mainFilePath : FilePath
        , mainModuleName : ModuleName
        , sourceDirectory : FilePath
    }


type alias ProjectToEmit =
    { output : FileContents }


type alias Modules expr =
    Dict_ ModuleName (Module expr)


type ModuleType
    = PlainModule
    | PortModule
    | EffectModule EffectMetadata


type alias EffectMetadata =
    {}


type Exposing
    = ExposingAll -- exposing (..)
    | ExposingSome (List ExposedItem) -- exposing (foo, Foo, Bar(..))


type ExposedItem
    = ExposedValue String -- exposing (foo)
    | ExposedType String -- exposing (Foo)
    | ExposedTypeAndAllConstructors String -- exposing (Foo(..))


type alias Module expr =
    -- TODO comments? doc comments?
    { -- TODO somewhere check that dependencies' exposing lists contain only what's in that module's exposing list
      dependencies : Dict_ ModuleName Dependency -- ie. imports. Maybe rename?
    , name : ModuleName
    , filePath : FilePath
    , topLevelDeclarations : Dict_ VarName (TopLevelDeclaration expr)
    , type_ : ModuleType
    , exposing_ : Exposing
    }


{-| All four possibilities of the Maybes make sense:

                   | exposing_ = Nothing | exposing_ = Just ...
    ---------------|---------------------|-------------------------------
    as_ = Nothing  | import Foo          | import Foo exposing (..)
    as_ = Just "F" | import Foo as F     | import Foo as F exposing (..)

-}
type alias Dependency =
    { moduleName : ModuleName
    , as_ : Maybe ModuleName
    , exposing_ : Maybe Exposing
    }


type VarName
    = VarName String


type alias TopLevelDeclaration expr =
    { name : VarName
    , body : expr
    , module_ : ModuleName
    }


type alias Binding expr =
    { name : VarName
    , body : expr
    }

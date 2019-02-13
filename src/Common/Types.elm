module Common.Types exposing
    ( Dependency
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
    )

{-| TODO is this an OK way to do the module hierarchy? Eg. the Types module.
Is there a better one?
-}

import AST.Backend as Backend
import AST.Canonical as Canonical
import AST.Common exposing (TopLevelDeclaration, VarName)
import AST.Frontend as Frontend
import Dict.Any as AnyDict exposing (AnyDict)
import Elm.Project
import Set.Any as AnySet exposing (AnySet)


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


type alias Project expr =
    { mainFilePath : FilePath
    , mainModuleName : ModuleName
    , elmJson : Elm.Project.Project
    , {- TODO allow multiple source directories -} sourceDirectory : FilePath
    , program : Modules expr
    }


type alias ProjectToEmit =
    { output : FileContents
    }


type alias Modules expr =
    Dict_ ModuleName (Module expr)


type ModuleType
    = PlainModule
    | PortModule
    | EffectModule EffectMetadata


type alias EffectMetadata =
    -- TODO figure effect managers out
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
    { dependencies : Dict_ ModuleName Dependency -- ie. imports. Maybe rename?
    , name : ModuleName
    , filePath : FilePath
    , topLevelDeclarations : Dict_ VarName (TopLevelDeclaration expr)
    , type_ : ModuleType
    , exposing_ : Exposing
    }


{-| All four possibilities of the Maybes make sense:

                   | exposing_ = Nothing | exposing_ = Just ...
    ---------------|---------------------|-------------------------------
    as_ = Nothing  | import Foo         | import Foo exposing (..)
    as_ = Just "F" | import Foo as F    | import Foo as F exposing (..)

-}
type alias Dependency =
    { moduleName : ModuleName
    , as_ : Maybe ModuleName
    , exposing_ : Maybe Exposing
    }

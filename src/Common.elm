module Common exposing
    ( AST
    , Dict_
    , FileContents(..)
    , FilePath(..)
    , Module
    , ModuleName(..)
    , Project
    , Set_
    , filePathToString
    , moduleNameToString
    )

import Dict.Any as AnyDict exposing (AnyDict)
import Set exposing (Set)
import Set.Any as AnySet exposing (AnySet)


type alias Set_ a =
    AnySet String a


type alias Dict_ a b =
    AnyDict String a b


type FilePath
    = FilePath String


filePathToString : FilePath -> String
filePathToString (FilePath filePath) =
    filePath


type ModuleName
    = ModuleName String


moduleNameToString : ModuleName -> String
moduleNameToString (ModuleName moduleName) =
    moduleName


type FileContents
    = FileContents String


{-| TODO
-}
type alias AST =
    {}


{-| TODO
-}
type alias Project =
    {}


{-| TODO
-}
type alias Module =
    { dependencies :
        -- TODO will have to contain the `as` and `exposing` information later
        Set_ ModuleName
    , name : ModuleName
    , filePath : FilePath
    }

module Stage.Parse exposing (parse)

import Common exposing (FileContents(..), FilePath(..), Module)
import Error exposing (Error, ParseError(..))


type Name
    = Name String


type Expr
    = Var Name
    | Application
        { fn : Expr
        , arg : Expr
        }
    | Lambda
        { argName : Name
        , body : Expr
        }
    | Let
        { varName : Name
        , varBody : Expr
        , body : Expr
        }
    | Literal Literal
    | If
        { test : Expr
        , then_ : Expr
        , else_ : Expr
        }
    | Fixpoint Expr
    | Operator
        { opName : Name
        , left : Expr
        , right : Expr
        }


type Literal
    = LInt Int
      -- TODO | LFloat Float
    | LBool Bool -- TODO how to do this and have Bools defined in the elm/core instead of hardcoded in the compiler?


type alias TopLevelDeclaration =
    { name : Name
    , body : Expr
    }


type alias Program =
    { topLevelDeclarations : List TopLevelDeclaration
    , main : Expr
    }



-- TODO check filename too


{-| I suspect in the future we'll have to add an argument of previously parsed
modules.
-}
parse : FilePath -> FileContents -> Result Error Module
parse filePath fileContents =
    Debug.todo "parse"

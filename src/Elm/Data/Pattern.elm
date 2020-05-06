module Elm.Data.Pattern exposing (Pattern)

import Elm.Data.VarName as VarName exposing (VarName)


{-| Pattern docs

@docs Pattern

-}
type Pattern
    = Anything
    | Var VarName
    | Record (List VarName)
    | Alias Pattern VarName
    | Unit
    | Tuple Pattern Pattern (Maybe Pattern)
    | List_ (List Pattern)
    | Cons Pattern Pattern
      --| Bool Union Bool
    | Bool Bool
    | Chr Char
    | Str String
    | Int Int



--| Ctor
--    { _p_home : ModuleName.Canonical
--    , _p_type : Name
--    , _p_union : Union
--    , _p_name : Name
--    , _p_index : Index.ZeroBased
--    , _p_args : [PatternCtorArg]
--    }
--    -- CACHE _p_home, _p_type, and _p_vars for type inference
--    -- CACHE _p_index to replace _p_name in PROD code gen
--    -- CACHE _p_opts to allocate less in PROD code gen
--    -- CACHE _p_alts and _p_numAlts for exhaustiveness checker

module Elm.AST.Frontend exposing
    ( ProjectFields
    , LocatedExpr, Expr(..), unwrap, transform, recurse
    , LocatedPattern, Pattern(..), unwrapPattern
    )

{-| Frontend AST is the first stage after parsing from source code, and has
the closest resemblance to the Elm source code. Eg. all the comments etc. are
still there.

@docs ProjectFields
@docs LocatedExpr, Expr, unwrap, transform, recurse

-}

import Dict exposing (Dict)
import Elm.AST.Frontend.Unwrapped as Unwrapped
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Comment exposing (Comment)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Transform


{-| "What does this compiler stage need to store about the whole project?

(See [`Elm.Data.Project`](Elm.Data.Project).)

In this case, a dict of all the compiled Elm [modules](Elm.Data.Module#Module)
that hold [frontend AST expressions](#LocatedExpr).

-}
type alias ProjectFields =
    { modules : Dict ModuleName (Module LocatedExpr TypeAnnotation PossiblyQualified) }


{-| The main type of this module. Expression with [location metadata](Elm.Data.Located).

Note the underlying [`Expr`](#Expr) custom type recurses on this [`LocatedExpr`](#LocatedExpr) type,
so that the children also each have their location metadata.

If you want expressions without location metadata, look at [`unwrap`](#unwrap).

-}
type alias LocatedExpr =
    Located Expr


{-| -}
type Expr
    = Unit
    | Bool Bool
    | Int Int
    | Float Float
    | Char Char
    | String String
    | Var { qualifiedness : PossiblyQualified, name : VarName }
    | Argument VarName
    | Parenthesized
        { commentsBefore : List Comment
        , expr : LocatedExpr
        , commentsAfter : List Comment
        }
    | Tuple
        { commentsBefore : List Comment
        , expr : LocatedExpr
        , commentsAfter : List Comment
        }
        { commentsBefore : List Comment
        , expr : LocatedExpr
        , commentsAfter : List Comment
        }
    | Tuple3
        { commentsBefore : List Comment
        , expr : LocatedExpr
        , commentsAfter : List Comment
        }
        { commentsBefore : List Comment
        , expr : LocatedExpr
        , commentsAfter : List Comment
        }
        { commentsBefore : List Comment
        , expr : LocatedExpr
        , commentsAfter : List Comment
        }
    | Record
        (List
            { commentsBefore : List Comment
            , binding : Binding.Commented LocatedExpr
            , commentsAfter : List Comment
            }
        )
    | List
        (List
            { commentsBefore : List Comment
            , expr : LocatedExpr
            , commentsAfter : List Comment
            }
        )
    | Call
        { fn : LocatedExpr
        , comments : List Comment
        , argument : LocatedExpr
        }
    | Lambda
        { arguments :
            List
                { commentsBefore : List Comment
                , argument : VarName
                }
        , commentsAfterArguments : List Comment
        , commentsBeforeBody : List Comment
        , body : LocatedExpr
        }
    | Plus
        { left : LocatedExpr
        , commentsAfterLeft : List Comment
        , commentsBeforeRight : List Comment
        , right : LocatedExpr
        }
    | Cons
        { left : LocatedExpr
        , commentsAfterLeft : List Comment
        , commentsBeforeRight : List Comment
        , right : LocatedExpr
        }
    | ListConcat
        { left : LocatedExpr
        , commentsAfterLeft : List Comment
        , commentsBeforeRight : List Comment
        , right : LocatedExpr
        }
    | Let
        { bindings :
            List
                { commentsBefore : List Comment
                , binding : Binding.Commented LocatedExpr
                }
        , commentsAfterBindings : List Comment
        , commentsBeforeBody : List Comment
        , body : LocatedExpr
        }
    | If
        { commentsBeforeTest : List Comment
        , test : LocatedExpr
        , commentsAfterTest : List Comment
        , commentsBeforeThen : List Comment
        , then_ : LocatedExpr
        , commentsAfterThen : List Comment
        , commentsBeforeElse : List Comment
        , else_ : LocatedExpr
        }
    | Case
        { commentsBeforeTest : List Comment
        , test : LocatedExpr
        , commentsAfterTest : List Comment
        , branches :
            List
                { commentsBeforePattern : List Comment
                , pattern : LocatedPattern
                , commentsAfterPattern : List Comment
                , commentsBeforeBody : List Comment
                , body : LocatedExpr
                }
        }


type alias LocatedPattern =
    Located Pattern


type Pattern
    = PAnything
    | PUnit
    | PBool Bool
    | PInt Int
    | PFloat Float
    | PChar Char
    | PString String
    | PVar VarName
    | PParenthesized
        { commentsBefore : List Comment
        , pattern : LocatedPattern
        , commentsAfter : List Comment
        }
    | PTuple
        { commentsBefore : List Comment
        , pattern : LocatedPattern
        , commentsAfter : List Comment
        }
        { commentsBefore : List Comment
        , pattern : LocatedPattern
        , commentsAfter : List Comment
        }
    | PTuple3
        { commentsBefore : List Comment
        , pattern : LocatedPattern
        , commentsAfter : List Comment
        }
        { commentsBefore : List Comment
        , pattern : LocatedPattern
        , commentsAfter : List Comment
        }
        { commentsBefore : List Comment
        , pattern : LocatedPattern
        , commentsAfter : List Comment
        }
    | PRecord
        (List
            { commentsBefore : List Comment
            , varName : VarName
            , commentsAfter : List Comment
            }
        )
    | PList
        (List
            { commentsBefore : List Comment
            , pattern : LocatedPattern
            , commentsAfter : List Comment
            }
        )
    | PAlias
        { pattern : LocatedPattern
        , commentsAfterPattern : List Comment
        , commentsBeforeAlias : List Comment
        , alias : VarName
        }
    | PCons
        { left : LocatedPattern
        , commentsAfterLeft : List Comment
        , commentsBeforeRight : List Comment
        , right : LocatedPattern
        }


{-| A helper for the [Transform](/packages/Janiczek/transform/latest/) library.
Runs the given function recursively on all children.
-}
recurse : (Expr -> Expr) -> Expr -> Expr
recurse f expr =
    let
        f_ =
            Located.map f
    in
    case expr of
        Unit ->
            expr

        Bool _ ->
            expr

        Int _ ->
            expr

        Float _ ->
            expr

        Char _ ->
            expr

        String _ ->
            expr

        Var _ ->
            expr

        Argument _ ->
            expr

        Parenthesized item ->
            Parenthesized
                { item | expr = f_ item.expr }

        Tuple item1 item2 ->
            Tuple
                { item1 | expr = f_ item1.expr }
                { item2 | expr = f_ item2.expr }

        Tuple3 item1 item2 item3 ->
            Tuple3
                { item1 | expr = f_ item1.expr }
                { item2 | expr = f_ item2.expr }
                { item3 | expr = f_ item3.expr }

        Record bindings ->
            Record <|
                List.map
                    (\({ binding } as binding_) ->
                        { binding_ | binding = Binding.mapCommented f_ binding }
                    )
                    bindings

        List items ->
            List <|
                List.map
                    (\item_ -> { item_ | expr = f_ item_.expr })
                    items

        Call ({ fn, argument } as call) ->
            Call
                { call
                    | fn = f_ fn
                    , argument = f_ argument
                }

        Lambda ({ body } as lambda_) ->
            Lambda { lambda_ | body = f_ body }

        Plus ({ left, right } as plus) ->
            Plus
                { plus
                    | left = f_ left
                    , right = f_ right
                }

        Cons ({ left, right } as cons) ->
            Cons
                { cons
                    | left = f_ left
                    , right = f_ right
                }

        ListConcat ({ left, right } as listConcat) ->
            ListConcat
                { listConcat
                    | left = f_ left
                    , right = f_ right
                }

        Let ({ bindings, body } as let_) ->
            Let
                { let_
                    | bindings =
                        List.map
                            (\({ binding } as binding_) ->
                                { binding_ | binding = Binding.mapCommented f_ binding }
                            )
                            bindings
                    , body = f_ body
                }

        If ({ test, then_, else_ } as if_) ->
            If
                { if_
                    | test = f_ test
                    , then_ = f_ then_
                    , else_ = f_ else_
                }

        Case ({ test, branches } as case_) ->
            Case
                { case_
                    | test = f_ test
                    , branches =
                        List.map
                            (\({ body } as branch) ->
                                { branch | body = f_ body }
                            )
                            branches
                }


{-| [Transform](/packages/Janiczek/transform/latest/Transform#transformAll)
the expression using the provided function.

Start at the children, repeatedly apply on them until they stop changing,
then go up.

-}
transform : (Expr -> Expr) -> Expr -> Expr
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr


{-| Discard the [location metadata](Elm.Data.Located#Located).
-}
unwrap : LocatedExpr -> Unwrapped.Expr
unwrap expr =
    case Located.unwrap expr of
        Unit ->
            Unwrapped.Unit

        Bool bool ->
            Unwrapped.Bool bool

        Int int ->
            Unwrapped.Int int

        Float float ->
            Unwrapped.Float float

        Char char ->
            Unwrapped.Char char

        String string ->
            Unwrapped.String string

        Var var_ ->
            Unwrapped.Var var_

        Argument name ->
            Unwrapped.Argument name

        Parenthesized item ->
            unwrap item.expr

        Tuple item1 item2 ->
            Unwrapped.Tuple
                (unwrap item1.expr)
                (unwrap item2.expr)

        Tuple3 item1 item2 item3 ->
            Unwrapped.Tuple3
                (unwrap item1.expr)
                (unwrap item2.expr)
                (unwrap item3.expr)

        Record bindings ->
            Unwrapped.Record <|
                List.map
                    (.binding
                        >> Binding.fromCommented
                        >> Binding.map unwrap
                    )
                    bindings

        List list ->
            Unwrapped.List <|
                List.map (.expr >> unwrap) list

        Call { fn, comments, argument } ->
            Unwrapped.Call
                { fn = unwrap fn
                , argument = unwrap argument
                }

        Lambda { arguments, body } ->
            Unwrapped.Lambda
                { arguments = List.map .argument arguments
                , body = unwrap body
                }

        Plus { left, right } ->
            Unwrapped.Plus (unwrap left) (unwrap right)

        Cons { left, right } ->
            Unwrapped.Cons (unwrap left) (unwrap right)

        ListConcat { left, right } ->
            Unwrapped.ListConcat (unwrap left) (unwrap right)

        Let { bindings, body } ->
            Unwrapped.Let
                { bindings =
                    List.map
                        (.binding
                            >> Binding.fromCommented
                            >> Binding.map unwrap
                        )
                        bindings
                , body = unwrap body
                }

        If { test, then_, else_ } ->
            Unwrapped.If
                { test = unwrap test
                , then_ = unwrap then_
                , else_ = unwrap else_
                }

        Case { test, branches } ->
            Unwrapped.Case
                { test = unwrap test
                , branches =
                    List.map
                        (\{ pattern, body } ->
                            { pattern = unwrapPattern pattern
                            , body = unwrap body
                            }
                        )
                        branches
                }


{-| Discard the pattern [location metadata](Elm.Data.Located#Located).
-}
unwrapPattern : LocatedPattern -> Unwrapped.Pattern
unwrapPattern expr =
    case Located.unwrap expr of
        PAnything ->
            Unwrapped.PAnything

        PUnit ->
            Unwrapped.PUnit

        PBool bool ->
            Unwrapped.PBool bool

        PInt int ->
            Unwrapped.PInt int

        PFloat float ->
            Unwrapped.PFloat float

        PChar char ->
            Unwrapped.PChar char

        PString string ->
            Unwrapped.PString string

        PVar varName ->
            Unwrapped.PVar varName

        PParenthesized item ->
            unwrapPattern item.pattern

        PTuple item1 item2 ->
            Unwrapped.PTuple
                (unwrapPattern item1.pattern)
                (unwrapPattern item2.pattern)

        PTuple3 item1 item2 item3 ->
            Unwrapped.PTuple3
                (unwrapPattern item1.pattern)
                (unwrapPattern item2.pattern)
                (unwrapPattern item3.pattern)

        PRecord varNames ->
            Unwrapped.PRecord (List.map .varName varNames)

        PList items ->
            Unwrapped.PList <|
                List.map
                    (\{ pattern } -> unwrapPattern pattern)
                    items

        PAlias { pattern, alias } ->
            Unwrapped.PAlias (unwrapPattern pattern) alias

        PCons { left, right } ->
            Unwrapped.PCons
                (unwrapPattern left)
                (unwrapPattern right)

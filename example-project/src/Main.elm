port module Main exposing (Msg(..), Natural, main)

{-| The above declaration shows how to:

  - declare a port module
  - expose all the tags of a custom type (Msg)
  - expose only the type (Natural)

-}

-- Shows how to import everthing from a module (Html). It is recommended to avoid this syntax.
-- Shows how to create an alias for a module name (Events)
-- Shows how to import multiple modules into a single namespace (Math). Use this with great care as it can create confusion about the source of a function.

import Bar
import Foo as F exposing (foo)



-- CUSTOM TYPES


{-| Shows how to define a single variant custom type.
Exposing only the type and not the tags ensures that the values of the type are created only through functions that can enforce constrains.
-}
type Natural
    = Natural Int


{-| Shows how to define a function that creates a value for the above custom type
-}
fromInt : Int -> Natural
fromInt intValue =
    -- Tags of the custom types are also acting as functions that create the custom type.
    -- max function is defined in the Basics module from elm/core and is imported by default. See elm/core for a list of default imports.
    Natural (max intValue 0)


{-| Shows how to unpack custom type parameters. Works only if the type has a single variant.
-}
toInt : Natural -> Int
toInt (Natural value) =
    value


{-| Shows how to define a function in a pointfree style by composing two functions.
-}
toString : Natural -> String
toString =
    -- String.fromInt shows how to use a module that is imported by default.
    toInt >> String.fromInt


{-| Shows how to control the operations on the your custom type.
In this case the code makes sure you are not storing negative values inside the custom type
-}
addInt : Natural -> Int -> Natural
addInt natural intValue =
    let
        -- One ca unpack / destructure a custom type inside a let..in too
        (Natural value) =
            natural
    in
    fromInt (value + intValue)


{-| Adds 42 (the value is an Int written with HEX notation).
-}
addMeaning : Natural -> Natural
addMeaning (Natural value) =
    fromInt (value + 0x2A)


{-| Shows how to create a type alias for a type that extends records. This alias will extend any other record with the field `name`.
-}
type alias Named a =
    { a | name : String }


{-| Shows how to use the above type alias.
-}
type alias NamedValue a =
    Named { value : a }


{-| Shows how to use the values from an extensible record alias fields
-}
namedToHtml : Named a -> Html msg
namedToHtml { name } =
    text name


namedNaturalToHtml : NamedValue Natural -> Html msg
namedNaturalToHtml namedValue =
    div []
        [ namedToHtml namedValue
        , text ": "
        , text (toString namedValue.value)
        ]


{-| Shows how to create a phantom type
-}
type Unit a
    = Unit Int


{-| When adding two units, the type parameter must be the same.
-}
addUnit : Unit a -> Unit a -> Unit a
addUnit (Unit first) (Unit second) =
    Unit (first + second)


{-| A type to be used with the above Unit type
-}
type Meter
    = Meter


{-| A second type to be used with the above Unit type
-}
type Gram
    = Gram


twoMeters : Unit Meter
twoMeters =
    Unit 2


threeMeters : Unit Meter
threeMeters =
    Unit 3


fewGrams : Unit Gram
fewGrams =
    Unit 21


someMeters : Unit Meter
someMeters =
    -- This works because the two units match
    addUnit twoMeters threeMeters



{- This value will throw an error if uncommented
   impossibleAdd : Unit Meter
   impossibleAdd =
       -- This doesn't work because the types don't match
       addUnit fewGrams someMeters
-}
-- MODEL


{-| Shows how to tie a name to a record type.
-}
type alias Model =
    { count : Natural
    , namedCount : NamedValue Natural
    }


{-| Shows how to ignore a parameter you are not using
This purposefully shows a function without a type signature although top level functions and values should have type signatures.
-}
init _ =
    ( { count = Natural 0
      , namedCount =
            { name = "Natural", value = Natural 0 }
      }
    , Cmd.none
    )



-- UPDATE


{-| Shows how to give a new name to a more complex type
-}
type alias Naturals =
    List Natural


{-| Shows how to define a custom type with multiple variants
-}
type Msg
    = Increment
    | Decrement
    | AddNextTen
    | OnSubscription (Result String Naturals)


{-| Shows how to unpack a record parameter while still keeping the full parameter.
You can use a subset of the fields in the record if you only need certain fields.
This function type signature has been purpusefully spread over multiple lines to show that complex signatures need not be single line.
-}
update :
    Msg
    -> Model
    -> ( Model, Cmd msg )
update msg ({ count } as model) =
    case msg of
        Increment ->
            ( { model | count = addInt count 1 }, Cmd.none )

        Decrement ->
            -- Shows how to create a new scope with a let..in expression
            let
                -- values and function defined inside let..in can have type signatures although they usually don't
                newValue : Natural
                newValue =
                    addInt count -1

                -- this is how to use the Debug.log to check for a value
                _ =
                    Debug.log "newValue" newValue

                -- this shows that you can declare multiple _ values without the compiler complaining.
                -- attempting to use a named declaration multiple times will result in a compiler error
                _ =
                    newValue
                        -- adding the next line at the end of a declaration with result in it being logged to the JS console
                        |> Debug.log "newValue"
            in
            if newValue == count then
                -- Shows how to call a port
                ( model, reportError "There are no negative Natural numbers" )

            else
                ( { model | count = newValue }, Cmd.none )

        AddNextTen ->
            let
                addIntToCount =
                    -- Shows how to partially apply a function.
                    -- This is very useful in scopes where the first arguments stay the same
                    addInt count

                intCount =
                    toInt count

                addTen =
                    -- Shows how to use an infix operator as a prefix function.
                    -- This is useful when you want to partially apply the operator and use
                    -- the resulting function in a higher order function.
                    (+) 10

                nextTen =
                    -- Shows how to use an infix operator as an argument in a higher order function.
                    List.foldl (+) 0 (List.range intCount (addTen intCount))
            in
            ( { model | count = addIntToCount nextTen }, Cmd.none )

        -- Shows how to unpack a variant by matching against the contained variants
        OnSubscription (Ok naturals) ->
            case naturals of
                -- Shows how to pattern match on a List
                [] ->
                    -- Shows how to recursively call update in order to avoid duplicating code.
                    update (OnSubscription (Err "Received an empty list")) model

                -- Shows how to pattern match on a list with a fixed number of elements
                [ first ] ->
                    ( { model | count = first }, Cmd.none )

                -- Shows how to pattern match on a list with at least two elements.
                first :: second :: _ ->
                    ( { model | count = first }, Cmd.none )

        OnSubscription (Err error) ->
            ( model, reportError error )



-- VIEW


{-| Shows how to declare a String that spans multiple lines
-}
multiline : String
multiline =
    """
    This is a multiline string. 
    It will be displayed by spliting the lines into separate paragraphs. 
"""


{-| Shows how to define a tuple.
-}
initials : ( Char, Char )
initials =
    -- Show how to declare a Char type.
    ( 'J', 'D' )


view : Model -> Html Msg
view model =
    let
        namedCount =
            -- Shows how to access a field from a record by using a field accessor function
            .namedCount model

        -- Shows how to pattern match a tuple
        ( first, last ) =
            initials

        -- a helper function
        named value =
            { name = value }

        -- shows that record field accessors work on expressions too
        nameFromExpression =
            (named "Foo").name
    in
    main_ []
        [ button [ onClick Increment ] [ text "+1" ]

        -- Shows how to avoid parentheses by using the backwards pipe operator
        , div [] [ text <| toString model.count ]

        -- Shows how to used a function from a module without having to expose it in the import section.
        , div [] [ button [ Events.onClick Decrement ] [ text "-1" ] ]
        , button [ onClick AddNextTen ] [ text "Add Next Ten" ]
        , div [] [ namedNaturalToHtml namedCount ]
        , String.lines multiline
            |> List.map (\line -> p [] [ text line ])
            |> div []
        , footer [] [ text (String.fromChar first), text (String.fromChar last) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Listen to the incomming port only if condition in the model is met
    if toInt model.count < 5 then
        -- Show how to use an anonymous function (lambda expression)
        fromJS
            (\value ->
                case List.map String.toInt value of
                    [] ->
                        OnSubscription (Err "Received an empty list")

                    (Just int) :: _ ->
                        let
                            output =
                                -- This shows how to prepend an element to a list
                                fromInt int :: []
                        in
                        if int >= 0 then
                            OnSubscription (Ok output)

                        else
                            -- Shows how to structure a complex function application by using the "pipe" operator
                            ("Received a negative number: " ++ String.fromInt int)
                                |> Err
                                |> OnSubscription

                    -- Shows how to catch all remaining variants. Watch out for this pattern as it can create troubles.
                    _ ->
                        OnSubscription (Err "Received a list that started with a non-integer ")
            )

    else
        Sub.none



-- WIRING


{-| Signature for Browser.element is Program flags model msg.
The flags type argument here is the Unit type: ()
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


{-| Shows how to define an outgoing port
-}
port reportError : String -> Cmd msg


{-| Shows how to define an incomming port.
The first parameter is a function that takes the data received from JS and produces a message that the app understands.
-}
port fromJS : (List String -> msg) -> Sub msg



-- ADVANCED SYNTAX


{-| Elm also has special syntax for declaring WebGL shaders. See more about this at: <https://github.com/elm-explorations/webgl/>
-}
vertexShader : WebGL.Shader { a | coord : Math.Vec3, position : Math.Vec3 } { b | view : Math.Mat4 } { vcoord : Math.Vec2 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform   mat4 view;
varying   vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}
|]

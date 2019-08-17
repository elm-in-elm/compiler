# Implementing the `Cmd` type in elm

`Cmd`s are a fundamental type in elm.
However, what _is_ an elm `Cmd`?
The [official docs](https://package.elm-lang.org/packages/elm/core/1.0.2/Platform-Cmd) say

> A command is a way of telling Elm, “Hey, I want you to do this thing!” So if you want to send an HTTP request, you would need to command Elm to do it. Or if you wanted to ask for geolocation, you would need to command Elm to go get it.
>
> Every Cmd specifies (1) which effects you need access to and (2) the type of messages that will come back into your application.4

## The ananatomy of a `Cmd`

A `Cmd` variable in an elm program will have one of the following JavaScript structures.
Note that in the generated JavaScript the discriminants (`__2_NODE`, etc.) are replaced to reduce the size of the file.

1. A `Cmd` created in elm using ```batch : List (Cmd a) -> Cmd a```.

    `list` is generated JavaScript for an elm `List (Cmd a)`.

    ```js
    {
        $: __2_NODE,
        __bags: list
    };
    ```

2. A `Cmd` created in elm using `map : (a -> msg) -> Cmd a -> Cmd msg`.

    `tagger` is the elm function and `bag` is the `Cmd a`.

    ```js
    {
        $: __2_MAP,
        __func: tagger,
        __bag: bag
    }
    ```

3. A `Cmd` created using `port xxx : something -> Cmd never` or a command created by an elm event manager.

    `home` is the name of the port or the name of an elm event manager (for example `Task` for commands created using `Task.perform`).
    `value` is the data passed to the port or the data that an elm event manager attaches to the `Cmd` to use later.
    This explaination of `value` for event managers is wooly and confusing largely because I currently do not understand how event managers work, I hope to iterate back to this document after documenting event mangers.

    ```js
    {
        $: __2_LEAF,
        __home: home,
        __value: value
    };
    ```

4. A `Cmd` created using `none : Cmd.none`.

    This one is implemented in pure elm!

    ```elm
    none : Cmd msg
    none =
        batch []
    ```

## Implementing `Cmd`s in elm

Naively one may try the following

```elm
type Cmd a
    = Batch (List Cmd a)
    | Leaf String Problem1
    | Map (Problem2 -> a) (Cmd Problem 2)
```

However, there are two problems

1. The type parameter `a` in `Cmd a` describes the type of message the runtime can send to an elm problem **not** the type of the payload that a `Cmd` has.
    As a concrete example, calling `port xxx : String -> Cmd never` creates a `Cmd never` which contains a `String` payload but `String` does **not** appear in the type signature.

    Elm's type system does not allow a type to contain a parameterised type unless the paramterised type is a parameter of the type.
    Elm does not allow

    ```elm
    type Hider normalTypeParameter
        = Data normalTypeParameter hidenTypeParameter
    ```

2. The second problem is similar, a `Cmd Msg` produced by `Cmd.map func cmd` must store `cmd` (of type `Cmd Something`) and `func` (of type `Something -> Msg`).
    However, `Something` cannot appear in the type signature of `Cmd Msg`!

## `Sub`s and Bags

The good news is that a `Sub` variable is _identical_ to a `Cmd` variable in every way (apart from the differnce in elm types).
This is why elm kernel code talks about bags: both `Cmd Msg` or `Sub Msg` variables are simply "bags" of data that the runtime will "route" to the appropevent managers which _may_, having managed an effect using the data, send a variable of type `Msg` to the elm app's update function.

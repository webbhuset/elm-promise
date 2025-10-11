module Promise.State exposing
    ( State(..)
    , fromMaybe, fromResult
    , resolve, toMaybe, getError, toResult
    , markStale, setPending, map
    , isEmpty, isPending, isStale, isDone, isError
    , encode, decoder
    )

{-| States represent the lifecycle of an asynchronous value that is loaded through
`Promise` helpers.

@docs State


## Creating states

@docs fromMaybe, fromResult


## Conversions

@docs resolve, toMaybe, getError, toResult


## Modify state

@docs markStale, setPending, map


## Check states

@docs isEmpty, isPending, isStale, isDone, isError


## JSON Encoding/Decoding

@docs encode, decoder

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Track whether a value is loading, stale, finished, or has failed.


## Explanation of states

    Empty

No value has been requested yet

    Pending Nothing

State is loading, no value yet

    Pending (Just "old value")

State is reloading and we have an old value

    Stale "old value"

We have a value, but it should be refreshed

    Done "fresh value"

We have a fresh value

    Error "something went wrong"

There was a problem loading the value

-}
type State e a
    = Empty
    | Pending (Maybe a)
    | Stale a
    | Done a
    | Error e


{-| Convert a `Maybe` into a state, keeping any existing payload.

    fromMaybe (Just 10) == Done 10

    fromMaybe Nothing == Empty

-}
fromMaybe : Maybe a -> State e a
fromMaybe maybeValue =
    case maybeValue of
        Just a ->
            Done a

        Nothing ->
            Empty


{-| Convert a `Result` into a state, keeping successful payloads and failures.

    fromResult (Ok 5) == Done 5

-}
fromResult : Result e a -> State e a
fromResult result =
    case result of
        Ok a ->
            Done a

        Err e ->
            Error e


{-| Extract any known value from the state.

    toMaybe (Pending (Just 3)) == Just 3

-}
toMaybe : State e a -> Maybe a
toMaybe state =
    case state of
        Empty ->
            Nothing

        Pending a ->
            a

        Stale a ->
            Just a

        Done a ->
            Just a

        Error e ->
            Nothing


{-| Extract the error payload when the state is `Error`.

    getError (Error "timeout") == Just "timeout"

-}
getError : State e a -> Maybe e
getError state =
    case state of
        Error e ->
            Just e

        _ ->
            Nothing


{-| Turn a state back into a `Result`, using the provided error when nothing has finished yet.

    toResult "still loading" (Done 8) == Ok 8

-}
toResult : e -> State e a -> Result e a
toResult errorWhenEmpty state =
    case state of
        Empty ->
            Err errorWhenEmpty

        Pending Nothing ->
            Err errorWhenEmpty

        Pending (Just a) ->
            Ok a

        Stale a ->
            Ok a

        Done a ->
            Ok a

        Error e ->
            Err e


{-| Handle all possible states by providing a value for each case.
-}
resolve : b -> (e -> b) -> (a -> b) -> State e a -> b
resolve whenPending whenError whenValue state =
    case state of
        Empty ->
            whenPending

        Pending Nothing ->
            whenPending

        Pending (Just a) ->
            whenValue a

        Stale a ->
            whenValue a

        Done a ->
            whenValue a

        Error e ->
            whenError e


{-| Map state
-}
map : (a -> b) -> State e a -> State e b
map fn state =
    case state of
        Empty ->
            Empty

        Pending ma ->
            Pending (Maybe.map fn ma)

        Stale a ->
            Stale (fn a)

        Done a ->
            Done (fn a)

        Error e ->
            Error e


{-| Mark completed values as `Stale` to indicate they should be refreshed.

    markStale (Done "user") == Stale "user"

-}
markStale : State e a -> State e a
markStale state =
    case state of
        Done a ->
            Stale a

        _ ->
            state


{-| Set the state to `Pending`, keeping any existing value as a `Maybe`.

    setPending (Done 5) == Pending (Just 5)

-}
setPending : State e a -> State e a
setPending state =
    case state of
        Empty ->
            Pending Nothing

        Pending a ->
            Pending a

        Stale a ->
            Pending (Just a)

        Done a ->
            Pending (Just a)

        Error e ->
            Error e


{-| Check if a state is empty, meaning no value has been requested yet.

    isEmpty Empty == True

-}
isEmpty : State e a -> Bool
isEmpty state =
    case state of
        Empty ->
            True

        _ ->
            False


{-| Check if a state is currently pending.

    isPending (Pending Nothing) == True

-}
isPending : State e a -> Bool
isPending state =
    case state of
        Pending a ->
            True

        _ ->
            False


{-| Detect if the data is still usable but due for a refresh.

    isStale (Stale 1) == True

-}
isStale : State e a -> Bool
isStale state =
    case state of
        Stale a ->
            True

        _ ->
            False


{-| Tell whether the state contains a completed value.

    isDone (Done 42) == True

-}
isDone : State e a -> Bool
isDone state =
    case state of
        Done a ->
            True

        _ ->
            False


{-| Tell whether the state is an error.

    isError (Error "boom") == True

-}
isError : State e a -> Bool
isError state =
    case state of
        Error e ->
            True

        _ ->
            False


{-| Encode a state to JSON, given encoders for the error and value types.

    encode JE.string JE.int (Done 5) == "{" tag ":" Done "," value ":5}"

-}
encode : (e -> Value) -> (a -> Value) -> State e a -> Value
encode encodeError encodeValue state =
    case state of
        Empty ->
            JE.object
                [ ( "tag", JE.string "Empty" )
                ]

        Pending ma ->
            JE.object
                [ ( "tag", JE.string "Pending" )
                , ( "value", Maybe.map encodeValue ma |> Maybe.withDefault JE.null )
                ]

        Stale a ->
            JE.object
                [ ( "tag", JE.string "Stale" )
                , ( "value", encodeValue a )
                ]

        Done a ->
            JE.object
                [ ( "tag", JE.string "Done" )
                , ( "value", encodeValue a )
                ]

        Error e ->
            JE.object
                [ ( "tag", JE.string "Error" )
                , ( "value", encodeError e )
                ]


{-| Decode a state from JSON, given decoders for the error and value types.

    decoder JD.string JD.int "{" tag ":" Done "," value ":5}" == Ok (Done 5)

-}
decoder : Decoder e -> Decoder a -> Decoder (State e a)
decoder decodeError decodeValue =
    JD.field "tag" JD.string
        |> JD.andThen
            (\tag ->
                case tag of
                    "Empty" ->
                        JD.succeed Empty

                    "Pending" ->
                        JD.nullable decodeValue
                            |> JD.field "value"
                            |> JD.map Pending

                    "Stale" ->
                        JD.field "value" decodeValue
                            |> JD.map Stale

                    "Done" ->
                        JD.field "value" decodeValue
                            |> JD.map Done

                    "Error" ->
                        JD.field "value" decodeError
                            |> JD.map Error

                    _ ->
                        JD.fail ("Unknown tag: " ++ tag)
            )

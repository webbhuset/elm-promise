module Promise.State exposing
    ( fromResult
    , resolve, toMaybe, getError, toResult
    , setPending, map, andMap
    , isPending, isDone, isError
    , code
    , encode, decoder
    , State(..)
    )

{-| States represent the lifecycle of an asynchronous value that is loaded through
`Promise` helpers.

@docs FinalState


## Creating states

@docs fromResult


## Conversions

@docs resolve, toMaybe, getError, toResult


## Modify state

@docs setPending, map


## Check states

@docs isPending, isDone, isError


## Helpers

@docs code


## JSON Encoding/Decoding

@docs encode, decoder

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Track whether a value is loading, finished, or has failed.


## Explanation of states

    Pending Nothing

State is loading, no value yet

    Pending (Just "value")

State is loading and we have a value

    Done "fresh value"

We have a fresh value

    Error "something went wrong"

There was a problem loading the value

-}
type State e a
    = Pending (Maybe a)
    | Done a
    | Error e


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
        Pending a ->
            a

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


{-| Turn a state into a `Result`.

Provide a default `Result` to use when there is no value available.

    toResult (Ok 1) (Done 42) == Ok 42

    toResult (Ok 1) (Pending Nothing) == Ok 1

    toResult (Ok 1) (Error "Oh no!") == Err "Oh no!"

    toResult (Err "No answer") (Done 42) == Ok 42

    toResult (Err "No answer") (Pending Nothing) == Err "No answer"

    toResult (Err "No answer") (Error "Oh no!") == Err "Oh no!"

-}
toResult : Result e a -> State e a -> Result e a
toResult result state =
    case state of
        Pending Nothing ->
            result

        Pending (Just a) ->
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
        Pending Nothing ->
            whenPending

        Pending (Just a) ->
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
        Pending ma ->
            Pending (Maybe.map fn ma)

        Done a ->
            Done (fn a)

        Error e ->
            Error e


{-| Combine any number of states

    map2 f stateA stateB =
        Done f
            |> andMap stateA
            |> andMap stateB
-}
andMap : State e (a -> b) -> State e a -> State e b
andMap stateFn stateA =
    case stateFn of
        Pending mf ->
            case stateA of
                Pending ma ->
                    Pending (Maybe.map2 (\f a -> f a) mf ma)

                Done a ->
                    Pending (Maybe.map (\f -> f a) mf)

                Error e ->
                    Error e

        Done f ->
            map f stateA

        Error e ->
            Error e


{-| Set the state to `Pending`, keeping any existing value as a `Maybe`.

    setPending (Done 5) == Pending (Just 5)

-}
setPending : State e a -> State e a
setPending state =
    case state of
        Pending a ->
            Pending a

        Done a ->
            Pending (Just a)

        Error e ->
            Pending Nothing


{-| Check if a state is currently pending.

    isPending (Pending Nothing) == True

    isPending (Pending (Just 1)) == True

-}
isPending : State e a -> Bool
isPending state =
    case state of
        Pending a ->
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


{-| Return a code representing the state constructor.

Useful for CSS class names.

    code (Pending Nothing) == "state-pending"

    code (Error "oops") == "state-error"

    code (Done 5) == "state-done"

-}
code : State e a -> String
code state =
    case state of
        Error _ ->
            "state-error"

        Pending _ ->
            "state-pending"

        Done _ ->
            "state-done"


{-| Encode a state to JSON, given encoders for the error and value types.

    encode JE.string JE.int (Done 5)
        |> JE.encode 2
        == """{ "tag": "Done","value": 5}"""

-}
encode : (e -> Value) -> (a -> Value) -> State e a -> Value
encode encodeError encodeValue state =
    case state of
        Pending ma ->
            JE.object
                [ ( "tag", JE.string "Pending" )
                , ( "value", Maybe.map encodeValue ma |> Maybe.withDefault JE.null )
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

    JD.decodeString
        (decoder JD.string JD.int)
        """{ "tag": "Done","value": 5}"""
        == Ok (Done 5)

    JD.decodeString
        (decoder JD.string JD.int)
        """{ "tag": "Error","value": "oops"}"""
        == Ok (Error "oops")

-}
decoder : Decoder e -> Decoder a -> Decoder (State e a)
decoder decodeError decodeValue =
    JD.field "tag" JD.string
        |> JD.andThen
            (\tag ->
                case tag of
                    "Pending" ->
                        JD.nullable decodeValue
                            |> JD.field "value"
                            |> JD.map Pending

                    "Done" ->
                        JD.field "value" decodeValue
                            |> JD.map Done

                    "Error" ->
                        JD.field "value" decodeError
                            |> JD.map Error

                    _ ->
                        JD.fail ("Unknown tag: " ++ tag)
            )

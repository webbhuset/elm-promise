module Promise.State exposing (..)


type State e a
    = Empty
    | Pending (Maybe a)
    | Stale a
    | Done a
    | Error e


fromResult : Result e a -> State e a
fromResult result =
    case result of
        Ok a ->
            Done a

        Err e ->
            Error e


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


getError : State e a -> Maybe e
getError state =
    case state of
        Error e ->
            Just e

        _ ->
            Nothing


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


markStale : State e a -> State e a
markStale state =
    case state of
        Done a ->
            Stale a

        _ ->
            state


isPending : State e a -> Bool
isPending state =
    case state of
        Pending a ->
            True

        _ ->
            False


isStale : State e a -> Bool
isStale state =
    case state of
        Stale a ->
            True

        _ ->
            False


isDone : State e a -> Bool
isDone state =
    case state of
        Done a ->
            True

        _ ->
            False


isError : State e a -> Bool
isError state =
    case state of
        Error e ->
            True

        _ ->
            False

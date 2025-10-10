module Promise exposing
    ( Promise, succeed, fail, fromState
    , cmdWhenEmpty, cmdWhenEmptyInDict
    , map, andThen, andMap, map2, map3, map4, combine
    , mapMsg, mapError
    , nothingOnError, nothingOnErrorIf, toResult, toState
    , whenPending, whenError, recover
    , withModel, embedModel
    , whenResolved, update, runWith, run
    )

{-|


## A Promise type for managing asynchronous effects in Elm

@docs Promise, succeed, fail, fromState


## Create effectful promises

@docs cmdWhenEmpty, cmdWhenEmptyInDict


## Map and chain promises

@docs map, andThen, andMap, map2, map3, map4, combine


## Transform messages, errors, and states

@docs mapMsg, mapError


## Extract

@docs nothingOnError, nothingOnErrorIf, toResult, toState


## Handle specific states

@docs whenPending, whenError, recover


## Working with the Model

@docs withModel, embedModel


## Run updates when a promise resolves

@docs whenResolved, update, runWith, run

-}

import Dict exposing (Dict)
import Promise.State as State exposing (State(..))



-- PROMISE


{-| -}
type Promise model msg e a
    = Promise (model -> ( State e a, ( model, Cmd msg ) ))


{-| -}
succeed : a -> Promise model msg e a
succeed a =
    Promise
        (\model ->
            ( Done a, ( model, Cmd.none ) )
        )


{-| -}
fail : e -> Promise model msg e a
fail error =
    Promise
        (\model ->
            ( Error error, ( model, Cmd.none ) )
        )


{-| -}
fromState : State e a -> Promise model msg e a
fromState state =
    Promise
        (\model ->
            ( state, ( model, Cmd.none ) )
        )



-- PERFORM EFFECTS WHEN A STATE IS EMPTY


{-| -}
cmdWhenEmpty :
    (model -> State e a)
    -> (State e a -> model -> model)
    -> Promise model msg e (Cmd msg)
    -> Promise model msg e a
cmdWhenEmpty get set (Promise getCmd) =
    let
        handlePromisedCmd :
            Maybe a
            -> ( State e (Cmd msg), ( model, Cmd msg ) )
            -> ( State e a, ( model, Cmd msg ) )
        handlePromisedCmd pending ( state, ( model1, cmd1 ) ) =
            case state of
                Empty ->
                    ( Empty
                    , ( model1, cmd1 )
                    )

                Pending Nothing ->
                    ( Pending Nothing, ( model1, cmd1 ) )

                Pending (Just cmd2) ->
                    ( Pending Nothing
                    , ( model1
                      , Cmd.batch [ cmd1, cmd2 ]
                      )
                    )

                Stale cmd2 ->
                    ( Pending pending
                    , ( set (Pending pending) model1
                      , Cmd.batch [ cmd1, cmd2 ]
                      )
                    )

                Done cmd2 ->
                    ( Pending pending
                    , ( set (Pending pending) model1
                      , Cmd.batch [ cmd1, cmd2 ]
                      )
                    )

                Error e ->
                    ( Error e, ( model1, cmd1 ) )
    in
    Promise
        (\model0 ->
            case get model0 of
                Empty ->
                    handlePromisedCmd Nothing (getCmd model0)

                Stale a ->
                    handlePromisedCmd (Just a) (getCmd model0)

                state ->
                    ( state, ( model0, Cmd.none ) )
        )


{-| -}
cmdWhenEmptyInDict :
    comparable
    -> (model -> Dict comparable (State e a))
    -> (Dict comparable (State e a) -> model -> model)
    -> Promise model msg e (Cmd msg)
    -> Promise model msg e a
cmdWhenEmptyInDict key get set getCmd =
    cmdWhenEmpty
        (get >> Dict.get key >> Maybe.withDefault Empty)
        (\v m -> set (Dict.insert key v (get m)) m)
        getCmd


{-| -}
withModel : (model -> Promise model msg e a) -> Promise model msg e a
withModel f =
    Promise (\m -> unwrap (f m) m)


{-| -}
mapMsg : (msg1 -> msg2) -> Promise model msg1 e a -> Promise model msg2 e a
mapMsg mapFun (Promise promise) =
    Promise
        (\model1 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            ( state, ( model2, Cmd.map mapFun cmd ) )
        )


{-| -}
mapError : (e1 -> e2) -> Promise model msg e1 a -> Promise model msg e2 a
mapError f (Promise promise) =
    Promise
        (\model1 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            ( case state of
                Empty ->
                    Empty

                Pending a ->
                    Pending a

                Stale a ->
                    Stale a

                Done a ->
                    Done a

                Error e ->
                    Error (f e)
            , ( model2, cmd )
            )
        )


{-| -}
whenPending : a -> Promise model msg e a -> Promise model msg e a
whenPending a (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            case state of
                Pending Nothing ->
                    ( Pending (Just a), ( model2, cmd ) )

                _ ->
                    ( state, ( model2, cmd ) )
        )


{-| -}
whenError : (e -> a) -> Promise model msg e a -> Promise model msg xx a
whenError errorToA (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            case state of
                Error e ->
                    ( Done (errorToA e), ( model2, cmd ) )

                Empty ->
                    ( Empty, ( model2, cmd ) )

                Pending a ->
                    ( Pending a, ( model2, cmd ) )

                Stale a ->
                    ( Stale a, ( model2, cmd ) )

                Done a ->
                    ( Done a, ( model2, cmd ) )
        )



-- EXTRACT


{-| -}
nothingOnError : Promise model msg e a -> Promise model msg xx (Maybe a)
nothingOnError =
    map Just >> whenError (always Nothing)


{-| -}
nothingOnErrorIf : (e -> Bool) -> Promise model msg e a -> Promise model msg e (Maybe a)
nothingOnErrorIf isIgnorable =
    map Just
        >> recover
            (\e ->
                if isIgnorable e then
                    succeed Nothing

                else
                    fail e
            )


{-| -}
toResult : Promise model msg e a -> Promise model msg xx (Result e a)
toResult =
    map Ok >> whenError Err


{-| -}
toState : Promise model msg e a -> Promise model msg xx (State e a)
toState (Promise promise) =
    Promise
        (\model1 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            ( case state of
                Pending _ ->
                    Pending (Just state)

                _ ->
                    Done state
            , ( model2, cmd )
            )
        )


{-| -}
recover : (e -> Promise model msg x a) -> Promise model msg e a -> Promise model msg x a
recover recoverWith (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            case state of
                Empty ->
                    ( Empty, ( model2, cmd ) )

                Pending a ->
                    ( Pending a, ( model2, cmd ) )

                Stale a ->
                    ( Stale a, ( model2, cmd ) )

                Done a ->
                    ( Done a, ( model2, cmd ) )

                Error e ->
                    let
                        ( st1, ( m2, cmd2 ) ) =
                            unwrap (recoverWith e) model2
                    in
                    ( st1, ( m2, Cmd.batch [ cmd, cmd2 ] ) )
        )


{-| Use with promises for nested models
-}
embedModel :
    (outerModel -> innerModel)
    -> (innerModel -> outerModel -> outerModel)
    -> Promise innerModel msg e a
    -> Promise outerModel msg e a
embedModel get set (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, cmd ) ) =
                    promise (get model1)
            in
            ( state, ( set model2 model1, cmd ) )
        )


{-| -}
map : (a -> b) -> Promise model msg e a -> Promise model msg e b
map mapFun (Promise promise) =
    Promise
        (\model1 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            ( case state of
                Empty ->
                    Empty

                Pending Nothing ->
                    Pending Nothing

                Pending (Just a) ->
                    Pending (Just (mapFun a))

                Stale a ->
                    Stale (mapFun a)

                Done a ->
                    Done (mapFun a)

                Error e ->
                    Error e
            , ( model2, cmd )
            )
        )


{-| -}
andThen :
    (a -> Promise model msg e b)
    -> Promise model msg e a
    -> Promise model msg e b
andThen andThenFun (Promise promise) =
    Promise
        (\model0 ->
            let
                ( state1, ( model1, cmd1 ) ) =
                    promise model0

                next : a -> ( State e b, ( model, Cmd msg ) )
                next a =
                    let
                        ( state2, ( model2, cmd2 ) ) =
                            unwrap (andThenFun a) model1
                    in
                    ( state2, ( model2, Cmd.batch [ cmd1, cmd2 ] ) )
            in
            case state1 of
                Empty ->
                    ( Empty, ( model1, cmd1 ) )

                Pending Nothing ->
                    ( Pending Nothing, ( model1, cmd1 ) )

                Pending (Just a) ->
                    next a

                Stale a ->
                    next a

                Done a ->
                    next a

                Error e ->
                    ( Error e, ( model1, cmd1 ) )
        )


{-| -}
andMap :
    Promise model msg e a
    -> Promise model msg e (a -> b)
    -> Promise model msg e b
andMap (Promise promise1) (Promise promise2) =
    Promise
        (\model0 ->
            let
                ( state1, ( model1, cmd1 ) ) =
                    promise1 model0

                ( state2, ( model2, cmd2 ) ) =
                    promise2 model1

                cmds : Cmd msg
                cmds =
                    Cmd.batch [ cmd1, cmd2 ]

                applyFn : a -> ( State e b, ( model, Cmd msg ) )
                applyFn a =
                    case state2 of
                        Empty ->
                            ( Empty
                            , ( model2, cmds )
                            )

                        Pending Nothing ->
                            ( Pending Nothing
                            , ( model2, cmds )
                            )

                        Pending (Just fn) ->
                            ( Pending <| Just <| fn a
                            , ( model2, cmds )
                            )

                        Stale fn ->
                            ( Stale (fn a)
                            , ( model2, cmds )
                            )

                        Done fn ->
                            ( Done (fn a)
                            , ( model2, cmds )
                            )

                        Error e ->
                            ( Error e
                            , ( model2, cmds )
                            )
            in
            case state1 of
                Empty ->
                    ( Empty, ( model2, cmds ) )

                Pending Nothing ->
                    ( Pending Nothing, ( model2, cmds ) )

                Pending (Just a) ->
                    applyFn a

                Stale a ->
                    applyFn a

                Done a ->
                    applyFn a

                Error e ->
                    ( Error e, ( model2, cmds ) )
        )


{-| -}
map2 :
    (a -> b -> c)
    -> Promise model msg e a
    -> Promise model msg e b
    -> Promise model msg e c
map2 mapFun promise1 promise2 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2


{-| -}
map3 :
    (a -> b -> c -> d)
    -> Promise model msg e a
    -> Promise model msg e b
    -> Promise model msg e c
    -> Promise model msg e d
map3 mapFun promise1 promise2 promise3 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2
        |> andMap promise3


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> Promise model msg err a
    -> Promise model msg err b
    -> Promise model msg err c
    -> Promise model msg err d
    -> Promise model msg err e
map4 mapFun promise1 promise2 promise3 promise4 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2
        |> andMap promise3
        |> andMap promise4


{-| -}
combine : List (Promise model msg e a) -> Promise model msg e (List a)
combine =
    List.foldr (map2 (::)) (succeed [])


{-| -}
whenResolved :
    (Result e a -> model -> ( model, Cmd msg ))
    -> Promise model msg e a
    -> Promise model msg Never ()
whenResolved mapFun (Promise promise) =
    Promise
        (\model0 ->
            let
                ( state, ( model1, cmd1 ) ) =
                    promise model0
            in
            case state of
                Empty ->
                    ( Empty, ( model1, cmd1 ) )

                Pending Nothing ->
                    ( Pending Nothing, ( model1, cmd1 ) )

                Pending (Just a) ->
                    let
                        ( model2, cmd2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )

                Stale a ->
                    let
                        ( model2, cmd2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Stale (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )

                Done a ->
                    let
                        ( model2, cmd2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )

                Error e ->
                    let
                        ( model2, cmd2 ) =
                            mapFun (Err e) model1
                    in
                    ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )
        )


{-| -}
update :
    (State e a -> model -> ( model, Cmd msg ))
    -> Promise model msg e a
    -> Promise model msg Never ()
update updateFun (Promise promise) =
    Promise
        (\model0 ->
            let
                ( state, ( model1, cmd1 ) ) =
                    promise model0

                ( model2, cmd2 ) =
                    updateFun state model1
            in
            ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )
        )


{-| -}
runWith :
    model
    -> Promise model msg Never ()
    -> ( model, Cmd msg )
runWith model promise =
    run promise model


{-| -}
run :
    Promise model msg Never ()
    -> model
    -> ( model, Cmd msg )
run (Promise promise) model1 =
    let
        ( _, ( model2, cmd ) ) =
            promise model1
    in
    ( model2, cmd )


unwrap : Promise model msg e a -> (model -> ( State e a, ( model, Cmd msg ) ))
unwrap (Promise p) =
    p

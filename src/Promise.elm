module Promise exposing (..)

import Dict exposing (Dict)
import Promise.State as State exposing (State(..))


type Promise model msg e a
    = Promise (model -> ( State e a, ( model, Cmd msg ) ))


unwrapPromise : Promise model msg e a -> (model -> ( State e a, ( model, Cmd msg ) ))
unwrapPromise (Promise p) =
    p


withModel : (model -> Promise model msg e a) -> Promise model msg e a
withModel f =
    Promise (\m -> unwrapPromise (f m) m)


succeed : a -> Promise model msg e a
succeed a =
    Promise
        (\model ->
            ( Done a, ( model, Cmd.none ) )
        )


fail : e -> Promise model msg e a
fail error =
    Promise
        (\model ->
            ( Error error, ( model, Cmd.none ) )
        )


fromState : State e a -> Promise model msg e a
fromState state =
    Promise
        (\model ->
            ( state, ( model, Cmd.none ) )
        )


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


withState : Promise model msg e a -> Promise model msg x (State e a)
withState (Promise promise) =
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


whenPending : a -> Promise model msg e a -> Promise model msg e a
whenPending a (Promise promise) =
    Promise
        (\model1 ->
            let
                -- ( State e a, ( model, Cmd msg ) )
                ( state, ( model2, cmd ) ) =
                    promise model1
            in
            case state of
                Pending Nothing ->
                    ( Pending (Just a), ( model2, cmd ) )

                _ ->
                    ( state, ( model2, cmd ) )
        )


whenError : (e -> a) -> Promise model msg e a -> Promise model msg x a
whenError errorToA (Promise promise) =
    Promise
        (\model1 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
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


errorToResult : Promise model msg e a -> Promise model msg x (Result e a)
errorToResult =
    map Ok >> whenError Err


optional : Promise model msg e a -> Promise model msg x (Maybe a)
optional =
    map Just >> whenError (always Nothing)


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
                            unwrapPromise (recoverWith e) model2
                    in
                    ( st1, ( m2, Cmd.batch [ cmd, cmd2 ] ) )
        )


{-| Use with promises for nested models
-}
mapModel :
    (outerModel -> innerModel)
    -> (innerModel -> outerModel -> outerModel)
    -> Promise innerModel msg e a
    -> Promise outerModel msg e a
mapModel get set (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, cmd ) ) =
                    promise (get model1)
            in
            ( state, ( set model2 model1, cmd ) )
        )


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


andThen :
    (a -> Promise model msg e b)
    -> Promise model msg e a
    -> Promise model msg e b
andThen andThenFun (Promise promise) =
    Promise
        (\model0 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
                ( state1, ( model1, cmd1 ) ) =
                    promise model0

                next : a -> ( State e b, ( model, Cmd msg ) )
                next a =
                    let
                        -- ( State e a, ( model, Cmd msg ) ) =
                        ( state2, ( model2, cmd2 ) ) =
                            unwrapPromise (andThenFun a) model1
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


andMap :
    Promise model msg e a
    -> Promise model msg e (a -> b)
    -> Promise model msg e b
andMap (Promise promise1) (Promise promise2) =
    Promise
        (\model0 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
                ( state1, ( model1, cmd1 ) ) =
                    promise1 model0

                -- ( State e (a -> b), ( model, Cmd msg ) ) =
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


map2 :
    (a -> b -> c)
    -> Promise model msg e a
    -> Promise model msg e b
    -> Promise model msg e c
map2 mapFun promise1 promise2 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2


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


combine : List (Promise model msg e a) -> Promise model msg e (List a)
combine =
    List.foldr (map2 (::)) (succeed [])


whenResolved :
    (Result e a -> model -> ( model, Cmd msg ))
    -> Promise model msg e a
    -> Promise model msg Never ()
whenResolved mapFun (Promise promise) =
    Promise
        (\model0 ->
            let
                -- ( State e a, ( model, Cmd msg ) ) =
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
                        -- ( model, Cmd msg )
                        ( model2, cmd2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )

                Stale a ->
                    let
                        -- ( model, Cmd msg )
                        ( model2, cmd2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Stale (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )

                Done a ->
                    let
                        -- ( model, Cmd msg )
                        ( model2, cmd2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )

                Error e ->
                    let
                        -- ( model, Cmd msg )
                        ( model2, cmd2 ) =
                            mapFun (Err e) model1
                    in
                    ( Done (), ( model2, Cmd.batch [ cmd1, cmd2 ] ) )
        )


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


runWith :
    model
    -> Promise model msg Never ()
    -> ( model, Cmd msg )
runWith model promise =
    run promise model


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


runQueue :
    List q
    -> (q -> Promise model msg Never ())
    -> model
    -> ( List q, ( model, Cmd msg ) )
runQueue queue makePromise model =
    List.foldl
        (\q ( qs, ( m1, cmds ) ) ->
            let
                ( s, ( m2, cmd ) ) =
                    unwrapPromise (makePromise q) m1
            in
            case s of
                Done _ ->
                    ( qs, ( m2, cmd :: cmds ) )

                _ ->
                    ( q :: qs, ( m2, cmd :: cmds ) )
        )
        ( [], ( model, [] ) )
        queue
        |> Tuple.mapSecond (Tuple.mapSecond Cmd.batch)


type Queue request err response
    = Queue
        { queued : Dict String request
        , pending : Dict String request
        , responses : Dict String (State err response)
        }


queue_init : Queue request err response
queue_init =
    Queue
        { queued = Dict.empty
        , pending = Dict.empty
        , responses = Dict.empty
        }


queue_add : String -> request -> Queue request err response -> Queue request err response
queue_add id request (Queue queue) =
    case Dict.get id queue.responses of
        Just (Pending Nothing) ->
            Queue queue

        _ ->
            Queue
                { queue
                    | queued = Dict.insert id request queue.queued
                    , responses = Dict.insert id (Pending Nothing) queue.responses
                }


queue_gotResponse : String -> Result err response -> Queue request err response -> Queue request err response
queue_gotResponse id result (Queue queue) =
    Queue
        { queue
            | responses =
                Dict.insert id
                    (case result of
                        Ok response ->
                            Done response

                        Err err ->
                            Error err
                    )
                    queue.responses
            , queued = Dict.remove id queue.queued
            , pending = Dict.remove id queue.pending
        }


queue_state : String -> Queue request err response -> State err response
queue_state id (Queue queue) =
    case Dict.get id queue.responses of
        Just response ->
            response

        Nothing ->
            Empty


queue_runConcurrent :
    (request -> Promise model msg err (Cmd msg))
    -> Queue request err response
    -> model
    -> ( Queue request err response, ( model, Cmd msg ) )
queue_runConcurrent makeCmd (Queue queue) model =
    let
        ( nextQueue, m4, cmds1 ) =
            queue.queued
                |> Dict.foldl foldFn
                    ( { queue | queued = Dict.empty }
                    , model
                    , []
                    )

        foldFn key request ( queue1, m2, cmds ) =
            let
                ( state, ( m3, cmd ) ) =
                    unwrapPromise (makeCmd request) m2
            in
            case state of
                Done cmd2 ->
                    ( { queue1 | pending = Dict.insert key request queue1.pending }
                    , m3
                    , cmd2 :: cmd :: cmds
                    )

                Error err ->
                    ( { queue1
                        | responses = Dict.insert key (Error err) queue1.responses
                      }
                    , m3
                    , cmd :: cmds
                    )

                _ ->
                    ( { queue1 | queued = Dict.insert key request queue1.queued }
                    , m3
                    , cmd :: cmds
                    )
    in
    ( Queue nextQueue
    , ( m4, Cmd.batch cmds1 )
    )


queue_runSequential :
    (request -> Promise model msg err (Cmd msg))
    -> Queue request err response
    -> model
    -> ( Queue request err response, ( model, Cmd msg ) )
queue_runSequential makeCmd (Queue queue) model =
    let
        ( nextQueue, m4, cmds1 ) =
            if Dict.isEmpty queue.pending then
                case
                    queue.queued
                        |> Dict.toList
                        |> List.head
                of
                    Just ( key, request ) ->
                        foldFn
                            key
                            request
                            ( { queue | queued = Dict.remove key queue.queued }
                            , model
                            , []
                            )

                    Nothing ->
                        ( queue, model, [] )

            else
                ( queue, model, [] )

        foldFn key request ( queue1, m2, cmds ) =
            let
                ( state, ( m3, cmd ) ) =
                    unwrapPromise (makeCmd request) m2
            in
            case state of
                Done cmd2 ->
                    ( { queue1 | pending = Dict.insert key request queue1.pending }
                    , m3
                    , cmd2 :: cmd :: cmds
                    )

                Error err ->
                    ( { queue1
                        | responses = Dict.insert key (Error err) queue1.responses
                      }
                    , m3
                    , cmd :: cmds
                    )

                _ ->
                    ( { queue1 | queued = Dict.insert key request queue1.queued }
                    , m3
                    , cmd :: cmds
                    )
    in
    ( Queue nextQueue
    , ( m4, Cmd.batch cmds1 )
    )

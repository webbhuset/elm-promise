module Promise exposing
    ( Promise, succeed, fail, fromState
    , effectWhenEmpty, effectWhenEmptyInDict
    , map, andThen, andMap, map2, map3, map4, combine
    , mapMsg, mapError
    , nothingOnError, nothingOnErrorIf, toResult, withState
    , whenPending, whenError, recover
    , withModel, embedModel
    , whenResolved, update, runWith, run
    )

{-|


## A Promise type for managing asynchronous effects in Elm

@docs Promise, succeed, fail, fromState


## Create effectful promises

@docs effectWhenEmpty, effectWhenEmptyInDict


## Map and chain promises

@docs map, andThen, andMap, map2, map3, map4, combine


## Transform messages, errors, and states

@docs mapMsg, mapError


## Extract

@docs nothingOnError, nothingOnErrorIf, toResult, withState


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
type Promise model effect e a
    = Promise (model -> ( State e a, ( model, List effect ) ))


{-| -}
succeed : a -> Promise model effect e a
succeed a =
    Promise
        (\model ->
            ( Done a, ( model, [] ) )
        )


{-| -}
fail : e -> Promise model effect e a
fail error =
    Promise
        (\model ->
            ( Error error, ( model, [] ) )
        )


{-| -}
fromState : State e a -> Promise model effect e a
fromState state =
    Promise
        (\model ->
            ( state, ( model, [] ) )
        )



-- PERFORM EFFECTS WHEN A STATE IS EMPTY


{-| -}
effectWhenEmpty :
    (model -> State e a)
    -> (State e a -> model -> model)
    -> Promise model effect e effect
    -> Promise model effect e a
effectWhenEmpty get set (Promise getEffect) =
    Promise
        (\model0 ->
            case get model0 of
                Empty ->
                    effectWhenEmptyHelp set Nothing (getEffect model0)

                Stale a ->
                    effectWhenEmptyHelp set (Just a) (getEffect model0)

                state ->
                    ( state, ( model0, [] ) )
        )


effectWhenEmptyHelp :
    (State e a -> model -> model)
    -> Maybe a
    -> ( State e effect, ( model, List effect ) )
    -> ( State e a, ( model, List effect ) )
effectWhenEmptyHelp set pending ( state, ( model1, effects ) ) =
    case state of
        Empty ->
            ( Empty
            , ( model1, effects )
            )

        Pending Nothing ->
            ( Pending Nothing, ( model1, effects ) )

        Pending (Just newEffect) ->
            ( Pending Nothing
            , ( model1
              , newEffect :: effects
              )
            )

        Stale newEffect ->
            ( Pending pending
            , ( set (Pending pending) model1
              , newEffect :: effects
              )
            )

        Done newEffect ->
            ( Pending pending
            , ( set (Pending pending) model1
              , newEffect :: effects
              )
            )

        Error e ->
            ( Error e, ( model1, effects ) )


{-| -}
effectWhenEmptyInDict :
    comparable
    -> (model -> Dict comparable (State e a))
    -> (Dict comparable (State e a) -> model -> model)
    -> Promise model effect e effect
    -> Promise model effect e a
effectWhenEmptyInDict key get set getEffect =
    effectWhenEmpty
        (get >> Dict.get key >> Maybe.withDefault Empty)
        (\v m -> set (Dict.insert key v (get m)) m)
        getEffect


{-| -}
withModel : (model -> Promise model effect e a) -> Promise model effect e a
withModel f =
    Promise (\m -> unwrap (f m) m)


{-| -}
mapMsg : (effect1 -> effect2) -> Promise model effect1 e a -> Promise model effect2 e a
mapMsg mapFun (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            ( state, ( model2, List.map mapFun effects ) )
        )


{-| -}
mapError : (e1 -> e2) -> Promise model effect e1 a -> Promise model effect e2 a
mapError f (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
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
            , ( model2, effects )
            )
        )


{-| -}
whenPending : a -> Promise model effect e a -> Promise model effect e a
whenPending a (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            case state of
                Pending Nothing ->
                    ( Pending (Just a), ( model2, effects ) )

                _ ->
                    ( state, ( model2, effects ) )
        )


{-| -}
whenError : (e -> a) -> Promise model effect e a -> Promise model effect xx a
whenError errorToA (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            case state of
                Error e ->
                    ( Done (errorToA e), ( model2, effects ) )

                Empty ->
                    ( Empty, ( model2, effects ) )

                Pending a ->
                    ( Pending a, ( model2, effects ) )

                Stale a ->
                    ( Stale a, ( model2, effects ) )

                Done a ->
                    ( Done a, ( model2, effects ) )
        )



-- EXTRACT


{-| -}
nothingOnError : Promise model effect e a -> Promise model effect xx (Maybe a)
nothingOnError =
    map Just >> whenError (always Nothing)


{-| -}
nothingOnErrorIf : (e -> Bool) -> Promise model effect e a -> Promise model effect e (Maybe a)
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
toResult : Promise model effect e a -> Promise model effect xx (Result e a)
toResult =
    map Ok >> whenError Err


{-| -}
withState : Promise model effect e a -> Promise model effect xx (State e a)
withState (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            ( case state of
                Pending _ ->
                    Pending (Just state)

                _ ->
                    Done state
            , ( model2, effects )
            )
        )


{-| -}
recover : (e -> Promise model effect x a) -> Promise model effect e a -> Promise model effect x a
recover recoverWith (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            case state of
                Empty ->
                    ( Empty, ( model2, effects ) )

                Pending a ->
                    ( Pending a, ( model2, effects ) )

                Stale a ->
                    ( Stale a, ( model2, effects ) )

                Done a ->
                    ( Done a, ( model2, effects ) )

                Error e ->
                    let
                        ( st1, ( m2, e2 ) ) =
                            unwrap (recoverWith e) model2
                    in
                    ( st1, ( m2, effects ++ e2 ) )
        )


{-| Use with promises for nested models
-}
embedModel :
    (outerModel -> innerModel)
    -> (innerModel -> outerModel -> outerModel)
    -> Promise innerModel effect e a
    -> Promise outerModel effect e a
embedModel get set (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise (get model1)
            in
            ( state, ( set model2 model1, effects ) )
        )


{-| -}
map : (a -> b) -> Promise model effect e a -> Promise model effect e b
map mapFun (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
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
            , ( model2, effects )
            )
        )


{-| -}
andThen :
    (a -> Promise model effect e b)
    -> Promise model effect e a
    -> Promise model effect e b
andThen andThenFun (Promise promise) =
    Promise
        (\model0 ->
            let
                ( state1, ( model1, e1 ) ) =
                    promise model0

                next : a -> ( State e b, ( model, List effect ) )
                next a =
                    let
                        ( state2, ( model2, e2 ) ) =
                            unwrap (andThenFun a) model1
                    in
                    ( state2, ( model2, e1 ++ e2 ) )
            in
            case state1 of
                Empty ->
                    ( Empty, ( model1, e1 ) )

                Pending Nothing ->
                    ( Pending Nothing, ( model1, e1 ) )

                Pending (Just a) ->
                    next a
                        |> Tuple.mapFirst State.setPending

                Stale a ->
                    next a

                Done a ->
                    next a

                Error e ->
                    ( Error e, ( model1, e1 ) )
        )


{-| -}
andMap :
    Promise model effect e a
    -> Promise model effect e (a -> b)
    -> Promise model effect e b
andMap (Promise promise1) (Promise promise2) =
    Promise
        (\model0 ->
            let
                ( state1, ( model1, e1 ) ) =
                    promise1 model0

                ( state2, ( model2, e2 ) ) =
                    promise2 model1

                effects : List effect
                effects =
                    e1 ++ e2

                applyFn : a -> ( State e b, ( model, List effect ) )
                applyFn a =
                    case state2 of
                        Empty ->
                            ( Empty
                            , ( model2, effects )
                            )

                        Pending Nothing ->
                            ( Pending Nothing
                            , ( model2, effects )
                            )

                        Pending (Just fn) ->
                            ( Pending <| Just <| fn a
                            , ( model2, effects )
                            )

                        Stale fn ->
                            ( Stale (fn a)
                            , ( model2, effects )
                            )

                        Done fn ->
                            ( Done (fn a)
                            , ( model2, effects )
                            )

                        Error e ->
                            ( Error e
                            , ( model2, effects )
                            )
            in
            case state1 of
                Empty ->
                    ( Empty, ( model2, effects ) )

                Pending Nothing ->
                    ( Pending Nothing, ( model2, effects ) )

                Pending (Just a) ->
                    applyFn a

                Stale a ->
                    applyFn a

                Done a ->
                    applyFn a

                Error e ->
                    ( Error e, ( model2, effects ) )
        )


{-| -}
map2 :
    (a -> b -> c)
    -> Promise model effect e a
    -> Promise model effect e b
    -> Promise model effect e c
map2 mapFun promise1 promise2 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2


{-| -}
map3 :
    (a -> b -> c -> d)
    -> Promise model effect e a
    -> Promise model effect e b
    -> Promise model effect e c
    -> Promise model effect e d
map3 mapFun promise1 promise2 promise3 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2
        |> andMap promise3


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> Promise model effect err a
    -> Promise model effect err b
    -> Promise model effect err c
    -> Promise model effect err d
    -> Promise model effect err e
map4 mapFun promise1 promise2 promise3 promise4 =
    succeed mapFun
        |> andMap promise1
        |> andMap promise2
        |> andMap promise3
        |> andMap promise4


{-| -}
combine : List (Promise model effect e a) -> Promise model effect e (List a)
combine =
    List.foldr (map2 (::)) (succeed [])


{-| -}
whenResolved :
    (Result e a -> model -> ( model, List effect ))
    -> Promise model effect e a
    -> Promise model effect Never ()
whenResolved mapFun (Promise promise) =
    Promise
        (\model0 ->
            let
                ( state, ( model1, e1 ) ) =
                    promise model0
            in
            case state of
                Empty ->
                    ( Empty, ( model1, e1 ) )

                Pending Nothing ->
                    ( Pending Nothing, ( model1, e1 ) )

                Pending (Just a) ->
                    let
                        ( model2, e2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Done (), ( model2, e1 ++ e2 ) )

                Stale a ->
                    let
                        ( model2, e2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Stale (), ( model2, e1 ++ e2 ) )

                Done a ->
                    let
                        ( model2, e2 ) =
                            mapFun (Ok a) model1
                    in
                    ( Done (), ( model2, e1 ++ e2 ) )

                Error e ->
                    let
                        ( model2, e2 ) =
                            mapFun (Err e) model1
                    in
                    ( Done (), ( model2, e1 ++ e2 ) )
        )


{-| -}
update :
    (State e a -> model -> ( model, List effect ))
    -> Promise model effect e a
    -> Promise model effect Never ()
update updateFun (Promise promise) =
    Promise
        (\model0 ->
            let
                ( state, ( model1, e1 ) ) =
                    promise model0

                ( model2, e2 ) =
                    updateFun state model1
            in
            ( Done (), ( model2, e1 ++ e2 ) )
        )


{-| -}
runWith :
    model
    -> Promise model effect Never ()
    -> ( model, List effect )
runWith model promise =
    run promise model


{-| -}
run :
    Promise model effect Never ()
    -> model
    -> ( model, List effect )
run (Promise promise) model1 =
    let
        ( _, ( model2, effects ) ) =
            promise model1
    in
    ( model2, effects )


unwrap : Promise model effect e a -> (model -> ( State e a, ( model, List effect ) ))
unwrap (Promise p) =
    p

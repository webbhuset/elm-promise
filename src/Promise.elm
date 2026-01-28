module Promise exposing
    ( Promise
    , fromValue, fromError, fromResult, fromState
    , fromModel, fromUpdate, fromEffectWhenEmpty
    , map, andThen, andMap, map2, map3, map4, combine
    , mapEffect, mapError
    , withMaybe, withMaybeWhenError, withResult, withState
    , whenPending, whenError, recover
    , embedModel
    , update, runWith, run
    )

{-|


## A Promise type for managing asynchronous effects in Elm

@docs Promise


## Create promises

@docs fromValue, fromError, fromResult, fromState

@docs fromModel, fromUpdate, fromEffectWhenEmpty


## Map and chain promises

@docs map, andThen, andMap, map2, map3, map4, combine


## Transform effect, errors, and states

@docs mapEffect, mapError


## Extract

@docs withMaybe, withMaybeWhenError, withResult, withState


## Handle specific states

@docs whenPending, whenError, recover


## Working with the Model

@docs embedModel


## Run updates when a promise resolves

@docs update, runWith, run

-}

import Dict exposing (Dict)
import Promise.ModelState as ModelState
import Promise.State as State exposing (State(..))



-- PROMISE


{-| -}
type Promise model effect e a
    = Promise (model -> ( State e a, ( model, List effect ) ))


{-| -}
fromValue : a -> Promise model effect e a
fromValue a =
    Promise
        (\model ->
            ( Done a, ( model, [] ) )
        )


{-| -}
fromError : e -> Promise model effect e a
fromError error =
    Promise
        (\model ->
            ( Error error, ( model, [] ) )
        )


{-| -}
fromResult : Result e a -> Promise model effect e a
fromResult result =
    State.fromResult result
        |> fromState


{-| -}
fromState : State e a -> Promise model effect e a
fromState state =
    Promise
        (\model ->
            ( state, ( model, [] ) )
        )


{-| "Read" the `Model` to create a `Promise`.

For example when you need values from `Flags`.

-}
fromModel : (model -> Promise model effect e a) -> Promise model effect e a
fromModel f =
    Promise (\m -> unwrap (f m) m)


{-| Create a `Promise` based on the `Model`. Also allows you to update the `Model`.

Useful when you want to cache an expensive computation in the `Model`.

    parseSource : String -> Result Parser.Error AST
    parseSource source =
        -- expensive parsing logic here
        ...

    cachedAst : String -> Promise Model effect Parser.Error AST
    cachedAst source =
        fromUpdate
            (\model ->
                case Dict.get source model.astCache of
                    Just result ->
                        ( model
                        , Promise.fromResult result
                        )

                    Nothing ->
                        let
                            result =
                                parseSource source
                        in
                        ( { model
                            | astCache =
                                Dict.insert source
                                    result
                                    model.astCache
                          }
                        , Promise.fromResult result
                        )
            )

-}
fromUpdate : (model -> ( model, Promise model effect e a )) -> Promise model effect e a
fromUpdate fn =
    Promise
        (\model1 ->
            let
                ( model2, promise ) =
                    fn model1
            in
            unwrap promise model2
        )


{-| -}
fromEffects : State err a -> List effect -> Promise model effect err a
fromEffects state effects =
    Promise (\m -> ( state, ( m, effects ) ))


{-| Create a `Promise` based on values that are fetched via effects.

The effect could be anything like HTTP requests, ports, etc.

In this example, the effect is an HTTP request and it will only
be performed if the current state is `Empty` or `Stale`.

    type Msg
        = GotUser (Result Http.Error User)
        | OtherMessages

    getUser : Promise User
    getUser =
        Http.get
            { url = "/user"
            , expect =
                Http.expectJson
                    GotUser
                    userDecoder
            }
            |> Promise.fromValue
            |> Promise.fromEffectWhenEmpty
            |> Promise.embedModel
                .user
                (\state model ->
                    { model | user = state }
                )

-}
fromEffectWhenEmpty : Promise (ModelState.State err a) effect err effect -> Promise (ModelState.State err a) effect err a
fromEffectWhenEmpty getEffect =
    fromUpdate
        (\state ->
            case state of
                ModelState.Empty ->
                    ( ModelState.Pending Nothing
                    , getEffect
                        |> andThen (\effect -> fromEffects (State.Pending Nothing) [ effect ])
                    )

                ModelState.Stale a ->
                    ( ModelState.Pending (Just a)
                    , getEffect
                        |> andThen (\effect -> fromEffects (State.Pending (Just a)) [ effect ])
                    )

                ModelState.Done a ->
                    ( ModelState.Done a
                    , fromState (State.Done a)
                    )

                ModelState.Pending a ->
                    ( ModelState.Pending a
                    , fromState (State.Pending a)
                    )

                ModelState.Error e ->
                    ( ModelState.Error e
                    , fromState (State.Error e)
                    )
        )


{-| -}
mapEffect : (effect1 -> effect2) -> Promise model effect1 e a -> Promise model effect2 e a
mapEffect mapFun (Promise promise) =
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
                Pending a ->
                    Pending a

                Done a ->
                    Done a

                Error e ->
                    Error (f e)
            , ( model2, effects )
            )
        )


{-| Use a fixed value while state is `Pending Nothing`. Can be used to provide partial results when fetching in steps.

    getFooAndBar : Promise { foo : Foo, bar : Maybe Bar }
    getFooAndBar =
        fetchFoo
            |> Promise.andThen
                (\foo ->
                    fetchBar
                        |> Promise.map
                            (\bar ->
                                { foo = foo
                                , bar = Just bar
                                }
                            )
                        |> Promise.whenPending
                            { foo = foo
                            , bar = Nothing
                            }
                )

-}
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


{-| Recover from an error with a fixed value.

    getNumber : Promise Int
    getNumber =
        fetchNumber
            |> Promise.whenError 0

-}
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

                Pending a ->
                    ( Pending a, ( model2, effects ) )

                Done a ->
                    ( Done a, ( model2, effects ) )
        )



-- EXTRACT


{-| -}
withMaybe : Promise model effect e a -> Promise model effect xx (Maybe a)
withMaybe =
    map Just >> whenError (always Nothing)


{-| -}
withMaybeWhenError : (e -> Bool) -> Promise model effect e a -> Promise model effect e (Maybe a)
withMaybeWhenError isIgnorable =
    map Just
        >> recover
            (\e ->
                if isIgnorable e then
                    fromValue Nothing

                else
                    fromError e
            )


{-| -}
withResult : Promise model effect e a -> Promise model effect xx (Result e a)
withResult =
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


{-| Recover from an error with another promise.

    getNumber : Promise Int
    getNumber =
        fetchNumber
            |> Promise.recover (\_ -> fetchBackupNumber)

-}
recover : (e -> Promise model effect x a) -> Promise model effect e a -> Promise model effect x a
recover recoverWith (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            case state of
                Pending a ->
                    ( Pending a, ( model2, effects ) )

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


{-| Map a promise value.

    getUppercaseString : Promise String
    getUppercaseString =
        fetchString
            |> Promise.map String.toUpper

-}
map : (a -> b) -> Promise model effect e a -> Promise model effect e b
map mapFun (Promise promise) =
    Promise
        (\model1 ->
            let
                ( state, ( model2, effects ) ) =
                    promise model1
            in
            ( case state of
                Pending Nothing ->
                    Pending Nothing

                Pending (Just a) ->
                    Pending (Just (mapFun a))

                Done a ->
                    Done (mapFun a)

                Error e ->
                    Error e
            , ( model2, effects )
            )
        )


{-| Chain a promise.

    getUser : Promise User
    getUser =
        fetchAuthToken
            |> Promise.andThen
                (\token ->
                    fetchUserWithToken token
                )

-}
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
                Pending Nothing ->
                    ( Pending Nothing, ( model1, e1 ) )

                Pending (Just a) ->
                    next a
                        |> Tuple.mapFirst State.setPending

                Done a ->
                    next a

                Error e ->
                    ( Error e, ( model1, e1 ) )
        )


{-| Compose any number of promises.

    type alias Checkout =
        { product : Product
        , category : Category
        , cart : Cart
        }

    checkout : Promise Checkout
    checkout =
        Promise.fromValue Checkout
            |> Promise.andMap (productById productId)
            |> Promise.andMap (categoryById categoryId)
            |> Promise.andMap currentCart

-}
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
            in
                ( State.andMap state2 state1, ( model2, effects ) )
        )


{-| Compose two promises.

    getFooAndBar : Promise { foo : Foo, bar : Bar }
    getFooAndBar =
        Promise.map2
            (\foo bar ->
                { foo = foo
                , bar = bar
                }
            )
            fetchFoo
            fetchBar

-}
map2 :
    (a -> b -> c)
    -> Promise model effect e a
    -> Promise model effect e b
    -> Promise model effect e c
map2 mapFun promise1 promise2 =
    fromValue mapFun
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
    fromValue mapFun
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
    fromValue mapFun
        |> andMap promise1
        |> andMap promise2
        |> andMap promise3
        |> andMap promise4


{-| Combine a list of promises.

    getItems : List String -> Promise (List Item)
    getItems ids =
        ids
            |> List.map fetchItem
            |> Promise.combine

-}
combine : List (Promise model effect e a) -> Promise model effect e (List a)
combine =
    List.foldr (map2 (::)) (fromValue [])


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

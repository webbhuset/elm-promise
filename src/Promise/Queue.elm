module Promise.Queue exposing
    ( Queue, RequestId(..)
    , empty, add, remove, insert
    , requests, any, all
    , run, Group, send, withGroup, skip
    )

{-|

@docs Queue, RequestId

@docs empty, add, remove, insert

@docs requests, any, all

@docs run, Group, send, withGroup, skip

-}

import Promise exposing (Promise)
import Promise.State as State exposing (State)
import Set exposing (Set)


{-| -}
type RequestId
    = RequestId String


{-| -}
type Queue reqest
    = Queue (I_Queue reqest)


type alias I_Queue r =
    { requests : List ( RequestId, r )
    , nextId : Int
    , prefix : String
    }


{-| -}
requests : Queue request -> List ( RequestId, request )
requests (Queue queue) =
    queue.requests


{-| -}
empty : String -> Queue req
empty prefix =
    { requests = []
    , nextId = 0
    , prefix = prefix
    }
        |> Queue


{-| -}
add : req -> Queue req -> Queue req
add request (Queue queue) =
    let
        id =
            (queue.prefix ++ "-" ++ String.fromInt queue.nextId)
                |> RequestId
    in
    Queue
        { queue
            | requests =
                queue.requests ++ [ ( id, request ) ]
            , nextId = queue.nextId + 1
        }


{-| -}
remove : RequestId -> Queue req -> Queue req
remove id (Queue queue) =
    Queue
        { queue
            | requests =
                List.filter
                    (\( itemId, _ ) ->
                        itemId /= id
                    )
                    queue.requests
        }


{-| -}
insert : RequestId -> req -> Queue req -> Queue req
insert id newRequest (Queue queue) =
    Queue
        { queue
            | requests =
                List.map
                    (\( itemId, request ) ->
                        if itemId == id then
                            ( itemId, newRequest )

                        else
                            ( itemId, request )
                    )
                    queue.requests
        }


{-| -}
any : (request -> Bool) -> Queue request -> Bool
any predicate (Queue queue) =
    List.any (predicate << Tuple.second) queue.requests


{-| -}
all : (request -> Bool) -> Queue request -> Bool
all predicate (Queue queue) =
    List.all (predicate << Tuple.second) queue.requests



-- ATTEMPT QUEUE PROCESSING


{-| -}
run :
    (RequestId -> req -> Promise model eff err (Group req eff))
    -> Queue req
    -> Promise model eff err ( Queue req, List eff )
run handleReq (Queue queue) =
    queue.requests
        |> List.foldl
            (\( id, req ) ->
                Promise.andThen
                    (\( pp, groupsSending ) ->
                        handleReq id req
                            |> Promise.map
                                (\group ->
                                    case group of
                                        SendGroup name updatedReq eff ->
                                            if Set.member name groupsSending then
                                                ( ( ( id, req ), [] ) :: pp
                                                , groupsSending
                                                )

                                            else
                                                ( ( ( id, updatedReq ), [ eff ] ) :: pp
                                                , Set.insert name groupsSending
                                                )

                                        Send updatedReq eff ->
                                            ( ( ( id, updatedReq ), [ eff ] ) :: pp
                                            , groupsSending
                                            )

                                        StopGroup name ->
                                            ( ( ( id, req ), [] ) :: pp
                                            , Set.insert name groupsSending
                                            )

                                        Skip ->
                                            ( ( ( id, req ), [] ) :: pp
                                            , groupsSending
                                            )
                                )
                    )
            )
            (Promise.fromValue ( [], Set.empty ))
        |> Promise.map
            (Tuple.first
                >> List.reverse
                >> List.unzip
                >> Tuple.mapSecond List.concat
            )
        |> Promise.map
            (\( reqs, effects ) ->
                ( Queue { queue | requests = reqs }
                , effects
                )
            )


{-| -}
type Group request effect
    = SendGroup String request effect
    | StopGroup String
    | Send request effect
    | Skip


{-| -}
skip : Group request effect
skip =
    Skip


{-| -}
send :
    State err response
    -> Promise model eff err ( request, effect )
    -> Promise model eff err (Group request effect)
send state promise =
    case state of
        State.Empty ->
            promise
                |> Promise.withState
                |> Promise.map
                    (\s ->
                        case s of
                            State.Done ( r, e ) ->
                                Send r e

                            _ ->
                                Skip
                    )

        State.Pending _ ->
            promise
                |> Promise.map (\_ -> Skip)

        _ ->
            promise
                |> Promise.map (\_ -> Skip)


{-| -}
withGroup :
    String
    -> State err response
    -> Promise model eff err ( request, effect )
    -> Promise model eff err (Group request effect)
withGroup group state promise =
    case state of
        State.Empty ->
            promise
                |> Promise.withState
                |> Promise.map
                    (\s ->
                        case s of
                            State.Done ( r, e ) ->
                                SendGroup group r e

                            _ ->
                                Skip
                    )

        State.Pending _ ->
            promise
                |> Promise.map (\_ -> StopGroup group)

        _ ->
            promise
                |> Promise.map (\_ -> Skip)

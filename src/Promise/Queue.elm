module Promise.Queue exposing (..)

import Promise.State as State exposing (State)


type alias Queue req =
    { requests : List ( Int, req )
    , nextId : Int
    }


empty : Queue req
empty =
    { requests = []
    , nextId = 0
    }


add : req -> Queue req -> Queue req
add request queue =
    { queue
        | requests = queue.requests ++ [ ( queue.nextId, request ) ]
        , nextId = queue.nextId + 1
    }


remove : Int -> Queue req -> Queue req
remove id queue =
    { queue
        | requests =
            List.filter
                (\( itemId, _ ) ->
                    itemId /= id
                )
                queue.requests
    }


update : Int -> req -> Queue req -> Queue req
update id newRequest queue =
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


type Action r eff
    = Send ( r, eff )
    | Next
    | Wait


sendNext : (Int -> r -> Action r eff) -> Queue r -> ( Queue r, List eff )
sendNext toAction queue =
    walkRequests toAction queue.requests
        |> Tuple.mapFirst (\newRequests -> { queue | requests = newRequests })


walkRequests :
    (Int -> r -> Action r eff)
    -> List ( Int, r )
    -> ( List ( Int, r ), List eff )
walkRequests toAction requests =
    case requests of
        [] ->
            ( [], [] )

        ( id, request ) :: rest ->
            case toAction id request of
                Send ( updatedRequest, eff ) ->
                    ( ( id, updatedRequest ) :: rest
                    , [ eff ]
                    )

                Next ->
                    walkRequests toAction rest
                        |> Tuple.mapFirst (\newRest -> ( id, request ) :: newRest)

                Wait ->
                    ( requests, [] )


firstEmpty : State e a -> ( r, eff ) -> Action r eff
firstEmpty state pair =
    case state of
        State.Empty ->
            Send pair

        State.Pending _ ->
            Wait

        State.Stale _ ->
            Send pair

        State.Done _ ->
            Next

        State.Error _ ->
            Next

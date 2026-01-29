module QueueDemo exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Http
import Process
import Promise
import Promise.ModelState as ModelState
import Promise.Queue as Queue exposing (Queue)
import Promise.State as State exposing (State)
import Task exposing (Task)
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Promise e a =
    Promise.Promise Model (Cmd Msg) e a


type alias Product =
    { sku : String
    , name : String
    , price : Int
    }


testProducts =
    [ { sku = "12-123", name = "White Dress", price = 19 }
    , { sku = "13-456", name = "Hamburger", price = 29 }
    , { sku = "14-789", name = "A trip to Mars", price = 39 }
    ]
        |> List.map (\p -> ( p.sku, p ))
        |> Dict.fromList


type alias Cart =
    { items : List CartItem
    }


type alias CartItem =
    { id : String
    , sku : String
    , name : String
    , quantity : Int
    , rowTotal : Int
    }


type CartRequest
    = AddToCart
        { sku : String
        }
        (ModelState.State Http.Error Cart)
    | ChangeQuantity
        { id : String
        , quantity : Int
        }
        (ModelState.State Http.Error Cart)
    | Remove
        { id : String
        }
        (ModelState.State Http.Error Cart)


stateInResponse : CartRequest -> ModelState.State Http.Error Cart
stateInResponse cartRequest =
    case cartRequest of
        AddToCart _ state ->
            state

        ChangeQuantity _ state ->
            state

        Remove _ state ->
            state


processCartQueue : Model -> ( Model, Cmd Msg )
processCartQueue model =
    Queue.run
        attemptCartRequest
        model.cartQueue
        |> Promise.update
            (\state newModel ->
                case State.toMaybe state of
                    Just ( newQueue, cmds ) ->
                        ( { newModel
                            | cartQueue = newQueue
                            , cart =
                                Queue.requests newQueue
                                    |> List.foldl
                                        (\( _, req ) prevState ->
                                            case stateInResponse req of
                                                ModelState.Empty ->
                                                    ModelState.markStale prevState

                                                ModelState.Pending _ ->
                                                    ModelState.setPending prevState

                                                ModelState.Error _ ->
                                                    ModelState.toMaybe prevState
                                                        |> Maybe.map ModelState.Stale
                                                        |> Maybe.withDefault prevState

                                                ModelState.Stale cart ->
                                                    ModelState.Stale cart

                                                ModelState.Done cart ->
                                                    ModelState.Done cart
                                        )
                                        model.cart
                          }
                        , cmds
                        )

                    Nothing ->
                        Debug.todo "Handle error"
            )
        |> Promise.runWith model
        |> Tuple.mapSecond Cmd.batch


attemptCartRequest :
    Queue.RequestId
    -> CartRequest
    -> Promise Http.Error (Queue.Group CartRequest (Cmd Msg))
attemptCartRequest reqId cartRequest =
    case cartRequest of
        AddToCart req state ->
            sendToBackend reqId cartRequest
                |> Promise.map
                    (\cmd ->
                        ( AddToCart req (ModelState.Pending Nothing)
                        , cmd
                        )
                    )
                |> Queue.withGroup "cart" state

        ChangeQuantity { id, quantity } state ->
            Promise.fromValue Queue.skip

        Remove { id } state ->
            Promise.fromValue Queue.skip


sendToBackend :
    Queue.RequestId
    -> CartRequest
    -> Promise Http.Error (Cmd Msg)
sendToBackend reqId cartRequest =
    Process.sleep 1000
        |> Task.map
            (\_ ->
                MockBackend reqId cartRequest
            )
        |> Task.perform identity
        |> Promise.fromValue


type Msg
    = AddToCartClicked String
    | GotCartResponse Queue.RequestId CartRequest
    | RequestRowTrashClicked Queue.RequestId
    | MockBackend Queue.RequestId CartRequest


type alias Model =
    { products : List Product
    , cartQueue : Queue CartRequest
    , cart : ModelState.State Http.Error Cart
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { products = Dict.values testProducts ++ [ { sku = "15-404", name = "Http Error 404", price = 49 } ]
            , cartQueue = Queue.empty "cart-request"
            , cart = ModelState.Empty
            }
    in
    ( model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddToCartClicked sku ->
            { model
                | cartQueue =
                    Queue.add
                        (AddToCart { sku = sku } ModelState.Empty)
                        model.cartQueue
            }
                |> processCartQueue

        GotCartResponse reqId request ->
            { model
                | cartQueue = Queue.insert reqId request model.cartQueue
            }
                |> processCartQueue

        RequestRowTrashClicked reqId ->
            ( { model
                | cartQueue = Queue.remove reqId model.cartQueue
              }
            , Cmd.none
            )

        MockBackend reqId request ->
            ( model
            , respondUsingMockBackend model reqId request
            )


respondUsingMockBackend : Model -> Queue.RequestId -> CartRequest -> Cmd Msg
respondUsingMockBackend model reqId request =
    case request of
        AddToCart req _ ->
            case Dict.get req.sku testProducts of
                Nothing ->
                    ModelState.Error (Http.BadStatus 404)
                        |> AddToCart req
                        |> Task.succeed
                        |> Task.perform (GotCartResponse reqId)

                Just product ->
                    Time.now
                        |> Task.map
                            (\now ->
                                let
                                    cart =
                                        model.cart
                                            |> ModelState.toMaybe
                                            |> Maybe.withDefault { items = [] }

                                    existingItem =
                                        List.filter (\item -> item.sku == req.sku) cart.items
                                            |> List.head

                                    newList =
                                        case existingItem of
                                            Just item ->
                                                List.map
                                                    (\i ->
                                                        if i.sku == req.sku then
                                                            { i
                                                                | quantity = i.quantity + 1
                                                                , rowTotal = (i.quantity + 1) * product.price
                                                            }

                                                        else
                                                            i
                                                    )
                                                    cart.items

                                            Nothing ->
                                                cart.items
                                                    ++ [ { id = String.fromInt (Time.posixToMillis now)
                                                         , sku = req.sku
                                                         , name = product.name
                                                         , rowTotal = product.price
                                                         , quantity = 1
                                                         }
                                                       ]
                                in
                                { cart
                                    | items = newList
                                }
                                    |> ModelState.Done
                                    |> AddToCart req
                            )
                        |> Task.perform (GotCartResponse reqId)

        ChangeQuantity req _ ->
            ModelState.Error (Http.BadStatus 500)
                |> ChangeQuantity req
                |> Task.succeed
                |> Task.perform (GotCartResponse reqId)

        Remove req _ ->
            ModelState.Error (Http.BadStatus 500)
                |> Remove req
                |> Task.succeed
                |> Task.perform (GotCartResponse reqId)


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Html.node "style" [] [ Html.text css ]
        , view_ProductList model.products
        , arrowRight
        , view_RequestQueue model.cartQueue
        , arrowRight
        , view_CartSummary model
        ]


arrowRight =
    Html.section
        [ HA.class "arrow"
        ]
        [ Html.text "➟"
        ]


view_ProductList : List Product -> Html Msg
view_ProductList products =
    Html.section
        []
        [ Html.h2 [] [ Html.text "Products" ]
        , List.map
            (\product ->
                Html.li
                    [ HA.class "product"
                    ]
                    [ Html.h2 [] [ Html.text product.name ]
                    , Html.p [] [ Html.text product.sku ]
                    , Html.p [] [ Html.text ("$" ++ String.fromInt product.price) ]
                    , Html.button
                        [ Events.onClick (AddToCartClicked product.sku) ]
                        [ Html.text "Add to Cart" ]
                    ]
            )
            products
            |> Html.ul
                [ HA.class "table product-list"
                ]
        ]


view_CartSummary : Model -> Html Msg
view_CartSummary model =
    Html.section
        [ HA.class "cart"
        ]
        [ Html.h2
            [ HA.class (ModelState.code model.cart)
            ]
            [ Html.text "Shopping Cart" ]
        , model.cart
            |> ModelState.toMaybe
            |> Maybe.map .items
            |> Maybe.withDefault []
            |> (\items ->
                    if List.isEmpty items then
                        Html.p [] [ Html.text "Your cart is empty." ]

                    else
                        Html.ul
                            [ HA.class "table cart-items"
                            ]
                            (List.map
                                (\item ->
                                    Html.li
                                        [ HA.class "row"
                                        ]
                                        [ Html.div [] [ Html.text item.name ]
                                        , Html.div [] [ Html.text item.sku ]
                                        , Html.input
                                            [ HA.type_ "number"
                                            , HA.value (String.fromInt item.quantity)
                                            , HA.min "1"
                                            ]
                                            []
                                        , Html.div []
                                            [ Html.text
                                                ("$"
                                                    ++ String.fromInt item.rowTotal
                                                )
                                            ]
                                        ]
                                )
                                items
                            )
               )
        ]


view_RequestQueue : Queue CartRequest -> Html Msg
view_RequestQueue queue =
    let
        row state reqId attr html =
            Html.li
                (HA.class (State.code state)
                    :: HA.class "request-row"
                    :: attr
                )
                [ html
                , case state of
                    State.Error e ->
                        Html.code
                            []
                            [ Html.text (Debug.toString e) ]

                    _ ->
                        Html.code [] []
                , Html.button
                    [ Events.onClick (RequestRowTrashClicked reqId)
                    ]
                    [ Html.text "🗑️"
                    ]
                ]
    in
    Html.section
        [ HA.class "request-queue"
        ]
        [ Html.h2 [] [ Html.text "Request Queue" ]
        , Queue.requests queue
            |> List.map
                (\( reqId, cartRequest ) ->
                    case cartRequest of
                        AddToCart req state ->
                            row state
                                reqId
                                []
                                (Html.text ("Add to Cart: " ++ req.sku))

                        ChangeQuantity req state ->
                            row state
                                reqId
                                []
                                (Html.text
                                    ("Change Quantity: "
                                        ++ req.id
                                        ++ " to "
                                        ++ String.fromInt req.quantity
                                    )
                                )

                        Remove req state ->
                            row state
                                reqId
                                []
                                (Html.text ("Remove Item: " ++ req.id))
                )
            |> Html.ul
                [ HA.class "table request-list"
                ]
        ]


view_State :
    Html msg
    -> (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> (a -> List (Html msg))
    -> State e a
    -> Html msg
view_State emptyNode htmlNode attr viewDone state =
    case state of
        -- State.Empty ->
        --     htmlNode
        --         (HA.class "state-empty" :: attr)
        --         [ emptyNode
        --         ]
        State.Pending Nothing ->
            htmlNode
                (HA.class "state-pending" :: attr)
                [ Html.text "Loading..." ]

        State.Pending (Just lastSuccess) ->
            htmlNode
                (HA.class "state-pending" :: attr)
                (viewDone lastSuccess)

        State.Error err ->
            htmlNode
                (HA.class "state-error" :: attr)
                [ Html.code [] [ Html.text ("Error: " ++ Debug.toString err) ]
                ]

        -- State.Stale staleValue ->
        --     htmlNode
        --         (HA.class "state-stale" :: attr)
        --         (viewDone staleValue)
        State.Done value ->
            htmlNode
                (HA.class "state-done" :: attr)
                (viewDone value)


css : String
css =
    """
body {
    font-family: Arial, sans-serif;
    background-color: #111;
    color: #eee;
}

main {
    display: grid;
    grid-template-columns: 1fr max-content 1fr max-content 1fr;
    grid-template-rows: max-content auto;
    column-gap: 2em;
    row-gap: 1em;
    padding: 2em;
    & > section {
        display: grid;
        grid-template-rows: subgrid;
        grid-row: 1/-1;
    }
}
.arrow {
    font-size: 2rem;
    grid-row: 2;
    padding: 2em 0;
}

h1, h2, h3, p, ul, li, button {
    margin: 0;
    padding: 0;
}

.table {
    list-style: none;
    display: grid;
    align-content: start;
    column-gap: 1em;
    & > li {
        display: grid;
        grid-template-columns: subgrid;
        grid-column: 1/-1;
        align-items: center;
        padding: 0.3em;
        &:nth-child(odd) {
            background-color: #222;
        }
    }
}

.product-list {
    grid-template-columns: auto repeat(3, max-content);
    .product {
        h2, p {
            margin: 0;
            font-size: 1rem;
        }
        align-items: center;
    }
}

.cart {
    .cart-items {
        grid-template-columns: auto repeat(3, max-content);
        .row {
            input[type="number"] {
                text-align: right;
                width: 3em;
                background: #222;
                color: #eee;
                border: 1px solid #555;
            }
        }
    }
}

.request-queue {
    .request-list {
        grid-template-columns: max-content auto max-content max-content;
    }
    .request-row {
        button {
            background: none;
            border: none;
            color: #eee;
            font-size: 1rem;
            cursor: pointer;
        }
    }
}
.state-empty::before {
    content: "⏱️";
}
.state-pending::before {
    content: "⏳";
}
.state-error::before {
    content: "❌";
}
.state-stale::before {
    content: "⚠️";
}
.state-done::before {
    content: "✅";
}
"""

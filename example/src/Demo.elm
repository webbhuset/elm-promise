module Demo exposing (..)

{-| A Demo application showcasing the use of the Elm Promise library to handle
asynchronous data fetching with state management and integration with the Elm architecture.
-}

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Http
import Json.Decode as Decode
import Process
import Promise
import Promise.State as State exposing (State)
import Task exposing (Task)


{-| Declare a local alias for Promise to avoid having to
repeat the full type signature everywhere.
-}
type alias Promise a =
    Promise.Promise Model (Cmd Msg) Http.Error a



-- REMOTE CALLS


{-| The response from our backend that we can receive through
the `update` function.
-}
type Response
    = GotToUpper String (Result Http.Error String)
    | GotSuggestion String (Result Http.Error (List String))
    | GotUser (Result Http.Error User)


{-| In our Model we keep track of the state of our remote calls.

Note that we use Dicts to cache results for different input values.

-}
type alias RemoteModel a =
    { a
        | fromRemote_ToUpper : Dict String (State Http.Error String)
        , fromRemote_Suggestion : Dict String (State Http.Error (List String))
        , fromRemote_User : State Http.Error User
    }


type alias User =
    { name : String
    }


{-| A mock remote call that converts a string to uppercase. If the string starts with "error", it simulates an error response.

The State is stored in a Dict to cache results for each input string.

-}
fromRemote_ToUpper : String -> Promise String
fromRemote_ToUpper str =
    fetch_ToUpper str
        |> Promise.fromValue
        |> Promise.fromEffectWhenEmpty
        |> Promise.embedModel
            (.fromRemote_ToUpper
                >> Dict.get str
                >> Maybe.withDefault State.Empty
            )
            (\state model ->
                { model
                    | fromRemote_ToUpper =
                        Dict.insert
                            str
                            state
                            model.fromRemote_ToUpper
                }
            )


{-| This would probably be a Http request in a real app.

Here we just simulate success and error responses based on the input string with a delay.
-}
fetch_ToUpper : String -> Cmd Msg
fetch_ToUpper str =
    (if String.startsWith "error" str then
        "(╯°□°）╯︵ ┻━┻"
            |> Http.BadBody
            |> Err

     else
        String.toUpper str
            |> Ok
    )
        |> GotToUpper str
        |> FromRemote
        |> sendWithDelay 500


{-| A mock remote call that provides autocomplete suggestions based on the input string.
-}
fromRemote_Suggestion : String -> Promise (List String)
fromRemote_Suggestion str =
    fetch_Suggestions str
        |> Promise.fromValue
        |> Promise.fromEffectWhenEmpty
        |> Promise.embedModel
            (.fromRemote_Suggestion
                >> Dict.get str
                >> Maybe.withDefault
                    State.Empty
            )
            (\state model ->
                { model
                    | fromRemote_Suggestion =
                        Dict.insert
                            str
                            state
                            model.fromRemote_Suggestion
                }
            )


fetch_Suggestions : String -> Cmd Msg
fetch_Suggestions term =
    [ term ++ " One"
    , term ++ " Two"
    , term ++ " Three"
    ]
        |> Ok
        |> GotSuggestion term
        |> FromRemote
        |> sendWithDelay 1000


{-| A mock remote call that fetches user data based on the username.
-}
fromRemote_User : String -> Promise User
fromRemote_User username =
    fetch_UserByUsername username
        |> Promise.fromValue
        |> Promise.fromEffectWhenEmpty
        |> Promise.embedModel
            .fromRemote_User
            (\state model ->
                { model
                    | fromRemote_User = state
                }
            )


fetch_UserByUsername : String -> Cmd Msg
fetch_UserByUsername username =
    { name = username }
        |> Ok
        |> GotUser
        |> FromRemote
        |> sendWithDelay 500



-- THE PAGE


type Page
    = Login
    | Search SearchPage


{-| The data needed for the Search page.

Note that `suggestions` is a remote state. This enables us to show
a loading indicator while fetching suggestions without the whole page
being in a loading state.

-}
type alias SearchPage =
    { suggestions : State Http.Error (List String)
    , user : User
    }


{-| Compose Promises to make your page.

Should be familiar if you have worked with Json.Decode

-}
getPage : Promise Page
getPage =
    Promise.fromModel
        (\model ->
            case model.fromHtml_SubmittedUsername of
                Just username ->
                    Promise.map2
                        (\autocompleteResult user ->
                            { suggestions = autocompleteResult
                            , user = user
                            }
                                |> Search
                        )
                        (if String.isEmpty model.fromHtml_LastSearchTerm then
                            Promise.fromValue State.Empty

                         else
                            fromRemote_ToUpper model.fromHtml_LastSearchTerm
                                |> Promise.andThen fromRemote_Suggestion
                                |> Promise.withState
                        )
                        (fromRemote_User username)

                Nothing ->
                    Promise.fromValue Login
        )


{-| Resolve the current page based on the model.

Will handle all pending promises and update the model accordingly.

-}
resolvePage : Model -> ( Model, Cmd Msg )
resolvePage model =
    getPage
        |> Promise.update
            (\state m ->
                ( { m | page = state }
                , []
                )
            )
        |> Promise.runWith model
        |> Tuple.mapSecond Cmd.batch


{-| Helper function to send a message back to the update function after a delay.
-}
sendWithDelay : Float -> Msg -> Cmd Msg
sendWithDelay delay msg =
    Process.sleep delay
        |> Task.perform (\_ -> msg)



-- APP


type
    Msg
    -- Received a response from a remote call
    = FromRemote Response
    | FromHtml_SearchTerm String
    | FromHtml_LoginSubmit String
      -- Just a dummy message to show the initial state in the time travel debugger
    | AppStarted


{-| Our Model contains all remote states.
-}
type alias Model =
    RemoteModel
        { fromHtml_SubmittedUsername : Maybe String
        , fromHtml_LastSearchTerm : String

        -- The current page state. The page itself can also be in a pending/error state.
        , page : State Http.Error Page
        }


initModel : Flags -> Model
initModel flags =
    { fromRemote_ToUpper = Dict.empty
    , fromRemote_Suggestion = Dict.empty
    , fromRemote_User = State.Empty
    , fromHtml_SubmittedUsername = Nothing
    , fromHtml_LastSearchTerm = ""
    , page = State.Empty
    }


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = document
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            initModel flags
    in
    ( model
      -- Just a dummy command to show the initial state in the time travel debugger
    , sendWithDelay 0 AppStarted
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


{-| We need to run `resolvePage` on every update to make sure
that pending promises are handled.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppStarted ->
            resolvePage model

        FromRemote response ->
            -- When we receive a response, just insert it into the Model.
            case response of
                GotToUpper str result ->
                    { model
                        | fromRemote_ToUpper =
                            Dict.insert str
                                (State.fromResult result)
                                model.fromRemote_ToUpper
                    }
                        |> resolvePage

                GotSuggestion str result ->
                    { model
                        | fromRemote_Suggestion =
                            Dict.insert str
                                (State.fromResult result)
                                model.fromRemote_Suggestion
                    }
                        |> resolvePage

                GotUser result ->
                    { model
                        | fromRemote_User = State.fromResult result
                    }
                        |> resolvePage

        FromHtml_SearchTerm str ->
            { model | fromHtml_LastSearchTerm = str }
                |> resolvePage

        FromHtml_LoginSubmit str ->
            { model | fromHtml_SubmittedUsername = Just str }
                |> resolvePage



-- VIEW


document : Model -> Browser.Document Msg
document model =
    { title = "Elm Promise Example"
    , body =
        [ Html.node "style" [] [ Html.text css ]
        , view model
        ]
    }


view : Model -> Html Msg
view model =
    model.page
        |> view_State
            (Html.text "Empty")
            Html.main_
            [ HA.class "page"
            ]
            (\page ->
                case page of
                    Login ->
                        [ Html.section
                            [ HA.class "page-login"
                            ]
                            view_LoginForm
                        ]

                    Search searchPage ->
                        [ Html.section
                            [ HA.class "page-search"
                            ]
                            (view_SearchPage model searchPage)
                        ]
            )


view_LoginForm : List (Html Msg)
view_LoginForm =
    [ Html.form
        [ Events.preventDefaultOn "submit"
            (Decode.at [ "target", "elements", "username", "value" ] Decode.string
                |> Decode.map
                    (\username ->
                        ( FromHtml_LoginSubmit username
                        , True
                        )
                    )
            )
        ]
        [ Html.label
            []
            [ Html.div [] [ Html.text "Username" ]
            , Html.input
                [ HA.type_ "text"
                , HA.name "username"
                , HA.required True
                , HA.autofocus True
                , HA.placeholder "Enter your username"
                , HA.autocomplete False
                ]
                []
            ]
        , Html.button [ HA.type_ "submit" ] [ Html.text "Login" ]
        ]
    ]


view_SearchPage : Model -> SearchPage -> List (Html Msg)
view_SearchPage model searchPage =
    [ Html.h1 [] [ Html.text ("Hello, " ++ searchPage.user.name ++ "!") ]
    , Html.label
        []
        [ Html.div [] [ Html.text "Search" ]
        , Html.input
            [ HA.type_ "search"
            , Events.onInput FromHtml_SearchTerm
            , HA.value model.fromHtml_LastSearchTerm
            , HA.autofocus True
            ]
            []
        ]
    , Html.h2 [] [ Html.text "Autocomplete Suggestions" ]
    , view_State
        (Html.text "Type to see suggestions...")
        Html.div
        [ HA.class "suggestions-result"
        ]
        (\suggestions ->
            List.map
                (\suggestion ->
                    Html.li
                        []
                        [ Html.text suggestion ]
                )
                suggestions
                |> Html.ul []
                |> List.singleton
        )
        searchPage.suggestions
    ]


{-| A helper function to render different states.
-}
view_State :
    Html msg
    -> (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> (a -> List (Html msg))
    -> State e a
    -> Html msg
view_State emptyNode htmlNode attr viewDone state =
    case state of
        State.Empty ->
            htmlNode
                (HA.class "state-empty" :: attr)
                [ emptyNode
                ]

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

        State.Stale staleValue ->
            htmlNode
                (HA.class "state-stale" :: attr)
                (viewDone staleValue)

        State.Done value ->
            htmlNode
                (HA.class "state-done" :: attr)
                (viewDone value)


css : String
css =
    """
* {
    box-sizing: border-box;
    margin: 0;
}
body {
    font-family: sans-serif;
}
main {
    min-height: 100vh;
    display: grid;
    &.state-pending {
        justify-items: center;
        align-items: center;
    }
}
.state-empty {
    color: gray;
}
.state-pending {
    animation: blink 1s linear infinite;
}
.state-error {
    color: red;
}
@keyframes blink {
    0% {
        opacity: 1;
    }
    50% {
        opacity: 0.5;
    }
    100% {
        opacity: 1;
    }
}

.page-login {
    align-self: center;
    justify-self: center;
}
.page-search {
    padding: 1em;
    display: grid;
    gap: 1em;
    align-content: start;
}
"""

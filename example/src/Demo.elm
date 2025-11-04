module Demo exposing (..)

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


type alias Promise a =
    Promise.Promise Model (Cmd Msg) Http.Error a


fromRemote_ToUpper : String -> Promise String
fromRemote_ToUpper str =
    (if String.startsWith "error" str then
        "(╯°□°）╯︵ ┻━┻"
            |> Http.BadBody
            |> Err

     else
        String.toUpper str
            |> Ok
    )
        |> FromRemote_ToUpper str
        |> sendWithDelay 500
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


fromRemote_Suggestion : String -> Promise (List String)
fromRemote_Suggestion str =
    [ str ++ " 1", str ++ " 2", str ++ " 3" ]
        |> Ok
        |> FromRemote_Suggestion str
        |> sendWithDelay 1000
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


type alias User =
    { name : String
    }


fromRemote_User : String -> Promise User
fromRemote_User username =
    { name = username }
        |> Ok
        |> FromRemote_User
        |> sendWithDelay 500
        |> Promise.fromValue
        |> Promise.fromEffectWhenEmpty
        |> Promise.embedModel
            .fromRemote_User
            (\state model ->
                { model
                    | fromRemote_User = state
                }
            )



-- PAGE


type Page
    = Login
    | Search SearchPage


type alias SearchPage =
    { suggestions : State Http.Error (List String)
    , user : User
    }


getPage : Promise Page
getPage =
    Promise.fromModel
        (\model ->
            case model.fromHtml_Username of
                Just username ->
                    Promise.map2
                        (\autocompleteResult user ->
                            { suggestions = autocompleteResult
                            , user = user
                            }
                                |> Search
                        )
                        (if String.isEmpty model.fromHtml_SearchTerm then
                            Promise.fromValue State.Empty

                         else
                            fromRemote_ToUpper model.fromHtml_SearchTerm
                                |> Promise.andThen fromRemote_Suggestion
                                |> Promise.withState
                        )
                        (fromRemote_User username)

                Nothing ->
                    Promise.fromValue Login
        )


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


sendWithDelay : Float -> Msg -> Cmd Msg
sendWithDelay delay msg =
    Process.sleep delay
        |> Task.perform (\_ -> msg)



-- APP


type Msg
    = FromRemote_ToUpper String (Result Http.Error String)
    | FromRemote_Suggestion String (Result Http.Error (List String))
    | FromRemote_User (Result Http.Error User)
    | FromHtml_SearchTerm String
    | FromHtml_LoginSubmit String
    -- Just a dummy message to show the initial state in the time travel debugger
    | AppStarted


type alias Model =
    { fromRemote_ToUpper : Dict String (State Http.Error String)
    , fromRemote_Suggestion : Dict String (State Http.Error (List String))
    , fromRemote_User : State Http.Error User
    , fromHtml_Username : Maybe String
    , fromHtml_SearchTerm : String
    , page : State Http.Error Page
    }


initModel : Flags -> Model
initModel flags =
    { fromRemote_ToUpper = Dict.empty
    , fromRemote_Suggestion = Dict.empty
    , fromRemote_User = State.Empty
    , fromHtml_Username = Nothing
    , fromHtml_SearchTerm = ""
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
    , sendWithDelay 0 AppStarted
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppStarted ->
            resolvePage model

        FromRemote_ToUpper str result ->
            { model
                | fromRemote_ToUpper =
                    Dict.insert str
                        (State.fromResult result)
                        model.fromRemote_ToUpper
            }
                |> resolvePage

        FromRemote_Suggestion str result ->
            { model
                | fromRemote_Suggestion =
                    Dict.insert str
                        (State.fromResult result)
                        model.fromRemote_Suggestion
            }
                |> resolvePage

        FromRemote_User result ->
            { model
                | fromRemote_User = State.fromResult result
            }
                |> resolvePage

        FromHtml_SearchTerm str ->
            { model | fromHtml_SearchTerm = str }
                |> resolvePage

        FromHtml_LoginSubmit str ->
            { model | fromHtml_Username = Just str }
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
    view_State
        (Html.text "Empty")
        Html.main_
        [ HA.class "page"
        ]
        (\page ->
            case page of
                Login ->
                    [ view_LoginForm
                    ]

                Search searchPage ->
                    [ Html.h1 [] [ Html.text ("Hello, " ++ searchPage.user.name ++ "!") ]
                    , Html.label
                        []
                        [ Html.div [] [ Html.text "Search" ]
                        , Html.input
                            [ HA.type_ "search"
                            , Events.onInput FromHtml_SearchTerm
                            , HA.value model.fromHtml_SearchTerm
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
        )
        model.page


view_LoginForm : Html Msg
view_LoginForm =
    Html.section
        [ HA.class "login-form" ]
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


css =
    """
* {
    box-sizing: border-box;
    margin: 0;
}
body {
    font-family: sans-serif;
    padding: 1em;
}
main {
    display: grid;
    gap: 1em;
    max-width: 600px;
    margin: auto;
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
"""

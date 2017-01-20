module Main exposing (..)

import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Page
    = HomePage
    | AddTaskPage
    | ListTasksPage


type Frequency
    = Daily
    | Weekly
    | Monthly


freqOfString : String -> Result String Frequency
freqOfString str =
    case str of
        "daily" ->
            Ok Daily

        "weekly" ->
            Ok Weekly

        "monthly" ->
            Ok Monthly

        _ ->
            Err (str ++ " is not a valid Frequency")


freqToString : Frequency -> String
freqToString freq =
    case freq of
        Daily ->
            "daily"

        Weekly ->
            "weekly"

        Monthly ->
            "monthly"


type alias Task =
    { id : Int
    , title : String
    , tags : List String
    , freq : Frequency
    }


type alias Model =
    { tasks : List Task
    , page : Page
    }


fetchTasksUrl : String
fetchTasksUrl =
    "http://localhost:4000/tasks"


fetchTasks : Cmd Msg
fetchTasks =
    Http.get fetchTasksUrl (Json.Decode.list taskDecoder)
        |> Http.send FetchTasksDone


taskDecoder : Json.Decode.Decoder Task
taskDecoder =
    Json.Decode.map4 Task
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "tags" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "frequency" Json.Decode.string |> Json.Decode.andThen freqDecoder)


freqDecoder : String -> Json.Decode.Decoder Frequency
freqDecoder str =
    case freqOfString str of
        Ok freq ->
            Json.Decode.succeed freq

        Err msg ->
            Json.Decode.fail ("unable to decode frequency: " ++ msg)


init : ( Model, Cmd Msg )
init =
    ( { tasks = [], page = HomePage }, fetchTasks )


type Msg
    = Navigate Page
    | FetchTasksDone (Result Http.Error (List Task))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Cmd.none )

        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "Fetch error" error
            in
                ( model, Cmd.none )


navLink : Page -> Page -> String -> Html Msg
navLink currentPage linkTo linkText =
    let
        attrs =
            if currentPage == linkTo then
                [ class "active" ]
            else
                []
    in
        li attrs [ a [ onClick (Navigate linkTo) ] [ text linkText ] ]


myNavbar : Page -> Html Msg
myNavbar currentPage =
    navbar
        DefaultNavbar
        [ class "navbar-static-top" ]
        [ container
            [ navbarHeader
                []
                [ navbarHamburger "#navbar"
                , navbarBrand
                    [ onClick (Navigate HomePage) ]
                    [ text "timeslots" ]
                ]
            , navbarCollapse
                [ id "navbar" ]
                [ navbarList
                    NavbarNav
                    NavbarDefault
                    []
                    [ navLink currentPage HomePage "Home"
                    , navLink currentPage AddTaskPage "Add Task"
                    , navLink currentPage ListTasksPage "List Tasks"
                    ]
                ]
            ]
        ]


viewTask : Task -> Html Msg
viewTask task =
    tr []
        [ td [] [ text (toString task.id) ]
        , td [] [ text task.title ]
        , td [] [ text (freqToString task.freq) ]
        , td [] (List.intersperse ", " task.tags |> List.map text)
        ]


viewTasks : List Task -> Html Msg
viewTasks tasks =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ]
                , th [] [ text "Title" ]
                , th [] [ text "Frequency" ]
                , th [] [ text "Tags" ]
                ]
            ]
        , tbody [] (List.map viewTask tasks)
        ]


view : Model -> Html Msg
view model =
    case model.page of
        HomePage ->
            div []
                [ myNavbar HomePage
                ]

        AddTaskPage ->
            div []
                [ myNavbar AddTaskPage
                ]

        ListTasksPage ->
            div []
                [ myNavbar ListTasksPage
                , container
                    [ row
                        [ viewTasks model.tasks ]
                    ]
                ]

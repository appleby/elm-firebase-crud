module Main exposing (..)

import Html exposing (Html, a, div, text)
import Html.Events exposing (onClick)


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


type Frequency
    = Daily
    | Weekly
    | Monthly


type alias Task =
    { title : String
    , tags : List String
    , freq : Frequency
    }


type alias Model =
    { tasks : List Task
    , page : Page
    }


init : ( Model, Cmd Msg )
init =
    ( { tasks = [], page = HomePage }, Cmd.none )


type Msg
    = Navigate Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        HomePage ->
            div []
                [ text "This is the Homepage "
                , a [ onClick (Navigate HomePage) ] [ text "Home" ]
                , text " | "
                , a [ onClick (Navigate AddTaskPage) ] [ text "Add Task" ]
                ]

        AddTaskPage ->
            div []
                [ text "This is the AddTaskPage "
                , a [ onClick (Navigate HomePage) ] [ text "Home" ]
                , text " | "
                , a [ onClick (Navigate AddTaskPage) ] [ text "Add Task" ]
                ]

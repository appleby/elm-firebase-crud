module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)


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
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model.page of
        HomePage ->
            myNavbar HomePage

        AddTaskPage ->
            myNavbar AddTaskPage

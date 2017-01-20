module Main exposing (..)

import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Navigation exposing (Location)
import UrlParser exposing ((</>))


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Frequency
    = Daily
    | Weekly
    | Monthly


type alias TaskId =
    Int


type alias Task =
    { id : TaskId
    , title : String
    , tags : List String
    , freq : Frequency
    }


type alias Model =
    { tasks : List Task
    , route : Route
    }


type Route
    = HomeRoute
    | TasksRoute
    | TaskRoute TaskId
    | NotFoundRoute


routeToString : Route -> String
routeToString route =
    case route of
        HomeRoute ->
            "#"

        TasksRoute ->
            "#tasks"

        TaskRoute id ->
            "#tasks/" ++ (toString id)

        NotFoundRoute ->
            "#notfound"


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map TaskRoute (UrlParser.s "tasks" </> UrlParser.int)
        , UrlParser.map TasksRoute (UrlParser.s "tasks")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


initModel : Route -> Model
initModel route =
    { tasks = [], route = route }


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


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initRoute =
            parseLocation location
    in
        ( initModel initRoute, fetchTasks )


type Msg
    = OnLocationChange Location
    | NewUrl String
    | FetchTasksDone (Result Http.Error (List Task))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "Fetch error" error
            in
                ( model, Cmd.none )


navLink : Route -> Route -> String -> Html Msg
navLink currentRoute linkTo linkText =
    let
        attrs =
            if currentRoute == linkTo then
                [ class "active" ]
            else
                []
    in
        li attrs [ a [ href (routeToString linkTo) ] [ text linkText ] ]


myNavbar : Route -> Html Msg
myNavbar currentRoute =
    navbar
        DefaultNavbar
        [ class "navbar-static-top" ]
        [ container
            [ navbarHeader
                []
                [ navbarHamburger "#navbar"
                , navbarBrand
                    [ onClick (NewUrl "#") ]
                    [ text "timeslots" ]
                ]
            , navbarCollapse
                [ id "navbar" ]
                [ navbarList
                    NavbarNav
                    NavbarDefault
                    []
                    [ navLink currentRoute HomeRoute "Home"
                    , navLink currentRoute TasksRoute "List Tasks"
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


page : Model -> Html Msg
page model =
    case model.route of
        HomeRoute ->
            div [] []

        TasksRoute ->
            container
                [ row [ viewTasks model.tasks ] ]

        TaskRoute taskId ->
            div [] []

        NotFoundRoute ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ myNavbar model.route
        , page model
        ]

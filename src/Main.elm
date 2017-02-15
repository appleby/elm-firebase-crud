module Main exposing (..)

import Auth
import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Navigation exposing (Location)
import Ports exposing (..)
import Route exposing (Route(..))
import TaskAddEdit exposing (..)
import TaskList exposing (..)


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , update = updateWithSignInCheck
        , view = view
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initRoute =
            Route.parseLocation location
    in
        ( initModel initRoute, Cmd.none )


initModel : Route -> Model
initModel route =
    { route = route
    , authModel = Auth.initModel
    , taskListModel = TaskList.initModel
    , taskAddEditModel = TaskAddEdit.initModel
    }


type alias Model =
    { route : Route
    , authModel : Auth.Model
    , taskListModel : TaskList.Model
    , taskAddEditModel : TaskAddEdit.Model
    }


type Msg
    = OnLocationChange Location
    | Goto Route
    | AuthMsg Auth.Msg
    | TaskListMsg TaskList.Msg
    | TaskAddEditMsg TaskAddEdit.Msg


mount : Model -> Route -> ( Model, Cmd Msg )
mount model route =
    case route of
        TaskEditRoute taskId ->
            ( { model | taskAddEditModel = TaskAddEdit.initModel }
            , Cmd.map TaskAddEditMsg (TaskAddEdit.mountEditCmd taskId)
            )

        TaskAddRoute ->
            ( { model | taskAddEditModel = TaskAddEdit.initModel }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


authRequired : Msg -> Bool
authRequired msg =
    case msg of
        AuthMsg msg ->
            Auth.authRequired msg

        TaskListMsg msg ->
            TaskList.authRequired msg

        TaskAddEditMsg msg ->
            TaskAddEdit.authRequired msg

        Goto HomeRoute ->
            False

        OnLocationChange location ->
            case Route.parseLocation location of
                HomeRoute ->
                    False

                _ ->
                    True

        _ ->
            True


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AuthMsg (Auth.subscriptions model.authModel)
        , Sub.map TaskAddEditMsg (TaskAddEdit.subscriptions model.taskAddEditModel)
        , Sub.map TaskListMsg (TaskList.subscriptions model.taskListModel)
        ]


updateWithSignInCheck : Msg -> Model -> ( Model, Cmd Msg )
updateWithSignInCheck msg model =
    if Auth.signedOut model.authModel && authRequired msg then
        ( model, signIn () )
    else
        update msg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto newRoute ->
            ( model, Route.goto newRoute )

        OnLocationChange location ->
            let
                newRoute =
                    Route.parseLocation location
            in
                mount { model | route = newRoute } newRoute

        AuthMsg authMsg ->
            let
                ( subModel, subCmd ) =
                    Auth.update authMsg model.authModel
            in
                ( { model | authModel = subModel }
                , Cmd.map AuthMsg subCmd
                )

        TaskListMsg taskListMsg ->
            let
                ( subModel, subCmd ) =
                    TaskList.update taskListMsg model.taskListModel
            in
                ( { model | taskListModel = subModel }
                , Cmd.map TaskListMsg subCmd
                )

        TaskAddEditMsg taskAddEditMsg ->
            let
                ( subModel, subCmd ) =
                    TaskAddEdit.update taskAddEditMsg model.taskAddEditModel
            in
                ( { model | taskAddEditModel = subModel }
                , Cmd.map TaskAddEditMsg subCmd
                )


navLink : Route -> Route -> String -> Html Msg
navLink currentRoute linkTo linkText =
    let
        attrs =
            if currentRoute == linkTo then
                [ class "active" ]
            else
                []
    in
        li attrs [ a [ href (Route.toString linkTo) ] [ text linkText ] ]


navLinks : Route -> Auth.Model -> List (Html Msg)
navLinks currentRoute authModel =
    if Auth.signedIn authModel then
        [ navLink currentRoute TasksRoute "List Tasks"
        , navLink currentRoute TaskAddRoute "Add Task"
        ]
    else
        []


myNavbar : Route -> Auth.Model -> Html Msg
myNavbar currentRoute authModel =
    navbar
        DefaultNavbar
        [ class "navbar-static-top" ]
        [ container
            [ navbarHeader
                []
                [ navbarHamburger "#navbar"
                , navbarBrand
                    [ onClick (Goto HomeRoute) ]
                    [ text "timeslots" ]
                ]
            , navbarCollapse
                [ id "navbar" ]
                [ navbarList
                    NavbarNav
                    NavbarDefault
                    []
                    (navLinks currentRoute authModel)
                , Auth.signInOut authModel |> Html.map AuthMsg
                ]
            ]
        ]


emptyDiv : Html Msg
emptyDiv =
    div [] []


page : Model -> Html Msg
page model =
    case model.route of
        HomeRoute ->
            emptyDiv

        TasksRoute ->
            TaskList.viewTasks model.taskListModel |> Html.map TaskListMsg

        TaskAddRoute ->
            TaskAddEdit.view TaskAddEdit.AddTask model.taskAddEditModel |> Html.map TaskAddEditMsg

        TaskEditRoute taskId ->
            TaskAddEdit.view TaskAddEdit.SaveTask model.taskAddEditModel |> Html.map TaskAddEditMsg

        NotFoundRoute ->
            -- TODO: Display 404
            emptyDiv


view : Model -> Html Msg
view model =
    div []
        [ myNavbar model.route model.authModel
        , page model
        ]

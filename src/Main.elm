module Main exposing (main)

import Auth
import Bootstrap.Grid exposing (container, row)
import Bootstrap.Navbar exposing (..)
import Bootstrap.Page exposing (jumbotron)
import Html exposing (Html, a, div, h1, i, li, p, strong, text)
import Html.Attributes exposing (class, href, id)
import Navigation
import Ports exposing (signIn)
import Route exposing (Route(..))
import TaskAddEdit
import TaskList


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , update = updateWithSignInCheck
        , view = view
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initModel location, Cmd.none )


initModel : Navigation.Location -> Model
initModel location =
    { route = Route.parse location
    , location = location
    , authModel = Auth.initModel
    , taskListModel = TaskList.initModel
    , taskAddEditModel = TaskAddEdit.initModel
    }


type alias Model =
    { route : Route
    , location : Navigation.Location
    , authModel : Auth.Model
    , taskListModel : TaskList.Model
    , taskAddEditModel : TaskAddEdit.Model
    }


type Msg
    = OnLocationChange Navigation.Location
    | AuthMsg Auth.Msg
    | TaskListMsg TaskList.Msg
    | TaskAddEditMsg TaskAddEdit.Msg


mount : Model -> ( Model, Cmd Msg )
mount model =
    let
        mountAddEdit mountFn =
            let
                ( subModel, subCmd ) =
                    mountFn model.taskAddEditModel
            in
                ( { model | taskAddEditModel = subModel }
                , Cmd.map TaskAddEditMsg subCmd
                )
    in
        case model.route of
            TaskAddRoute ->
                mountAddEdit TaskAddEdit.mountAdd

            TaskEditRoute taskId ->
                mountAddEdit (flip TaskAddEdit.mountEdit taskId)

            TasksRoute ->
                let
                    ( subModel, subCmd ) =
                        TaskList.mount model.taskListModel
                in
                    ( { model | taskListModel = subModel }
                    , Cmd.map TaskListMsg subCmd
                    )

            HomeRoute ->
                ( model, Cmd.none )

            NotFoundRoute ->
                ( model, Cmd.none )


unmount : Model -> ( Model, Cmd Msg )
unmount model =
    case model.route of
        TaskAddRoute ->
            ( model, Cmd.none )

        TaskEditRoute _ ->
            ( model, Cmd.none )

        TasksRoute ->
            let
                ( subModel, subCmd ) =
                    TaskList.unmount model.taskListModel
            in
                ( { model | taskListModel = subModel }
                , Cmd.map TaskListMsg subCmd
                )

        HomeRoute ->
            ( model, Cmd.none )

        NotFoundRoute ->
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

        OnLocationChange location ->
            case Route.parse location of
                HomeRoute ->
                    False

                NotFoundRoute ->
                    False

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
        OnLocationChange location ->
            let
                route =
                    Route.parse location
            in
                mount { model | route = route, location = location }

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
        li attrs [ Route.linkTo linkTo [] [ text linkText ] ]


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
                    [ Route.toHref HomeRoute ]
                    [ text "elm-firebase-crud" ]
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


error404 : Html Msg
error404 =
    jumbotron []
        [ h1 []
            [ i [ class "fa fa-frown-o fa-lg" ] []
            , text " 404 not found"
            ]
        , p []
            [ text "The page you are attempting to navigate to does not exist." ]
        , p []
            [ text "Use your browser's back button, or go to our "
            , Route.linkTo HomeRoute [] [ text "Home Page" ]
            ]
        ]


overview : Html Msg
overview =
    jumbotron []
        [ h1 []
            [ text "elm-firebase-crud" ]
        , p [ class "lead" ]
            [ text "This is an example of an elm app using firebase for it's backend. It does not do anything useful." ]
        , p []
            [ text "You can sign in anonymously by clicking the "
            , strong [] [ text "Sign In" ]
            , text " link in the top-right corner. "
            , strong [] [ text "All user data will be deleted 30 minutes after login." ]
            , text " See the "
            , a [ href "https://github.com/appleby/elm-firebase-crud/" ] [ text "associated github repo" ]
            , text " for more info."
            ]
        ]


page : Model -> Html Msg
page model =
    let
        taskAddEditView msg =
            TaskAddEdit.view msg model.taskAddEditModel
                |> Html.map TaskAddEditMsg
    in
        case model.route of
            HomeRoute ->
                overview

            TasksRoute ->
                TaskList.view model.taskListModel |> Html.map TaskListMsg

            TaskAddRoute ->
                taskAddEditView TaskAddEdit.AddTask

            TaskEditRoute _ ->
                taskAddEditView TaskAddEdit.SaveTask

            NotFoundRoute ->
                error404


deleteDataWarning : Model -> Html Msg
deleteDataWarning model =
    if model.location.hostname == "elm-crud.firebaseapp.com" then
        div [ class "alert alert-warning" ]
            [ strong [] [ text "Warning! " ]
            , text "All data will be deleted 30 minutes after login."
            ]
    else
        div [] []


view : Model -> Html Msg
view model =
    div []
        [ myNavbar model.route model.authModel
        , container
            [ row [ deleteDataWarning model ]
            , row [ page model ]
            ]
        ]

module Main exposing (..)

import Auth
import Bootstrap.Buttons exposing (..)
import Bootstrap.Forms exposing (..)
import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)
import Data exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Navigation exposing (Location)
import Ports exposing (..)
import String
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
            parseLocation location
    in
        ( initModel initRoute, Cmd.none )


initModel : Route -> Model
initModel route =
    { route = route
    , apiPending = False
    , displayResult = Nothing
    , pendingTask = emptyTask
    , authModel = Auth.initModel
    , taskListModel = TaskList.initModel
    }


type alias DisplayResult =
    Result String String


type alias Model =
    { route : Route
    , apiPending : Bool
    , displayResult : Maybe DisplayResult
    , pendingTask : Task
    , authModel : Auth.Model
    , taskListModel : TaskList.Model
    }


resetPageState : Model -> Route -> Model
resetPageState model newRoute =
    let
        pending =
            case newRoute of
                TaskEditRoute taskId ->
                    TaskList.findTaskById taskId model.taskListModel
                        |> Maybe.withDefault emptyTask

                _ ->
                    emptyTask
    in
        { model
            | pendingTask = pending
            , apiPending = False
            , displayResult = Nothing
        }


type Msg
    = OnLocationChange Location
    | Goto Route
    | EditTaskTitle String
    | EditTaskTags String
    | EditTaskFrequency String
    | SaveTask
    | SaveTaskDone Bool
    | AddTask
    | AddTaskDone Bool
    | AuthMsg Auth.Msg
    | TaskListMsg TaskList.Msg


authRequired : Msg -> Bool
authRequired msg =
    case msg of
        AuthMsg msg ->
            Auth.authRequired msg

        TaskListMsg msg ->
            TaskList.authRequired msg

        Goto HomeRoute ->
            False

        OnLocationChange location ->
            case parseLocation location of
                HomeRoute ->
                    False

                _ ->
                    True

        _ ->
            True


updateWithSignInCheck : Msg -> Model -> ( Model, Cmd Msg )
updateWithSignInCheck msg model =
    if Auth.signedOut model.authModel && authRequired msg then
        ( model, signIn () )
    else
        update msg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        Goto newRoute ->
            ( model, goto newRoute )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                resetModel =
                    resetPageState model newRoute
            in
                ( { resetModel | route = newRoute }, Cmd.none )

        EditTaskTitle title ->
            let
                pending =
                    model.pendingTask
            in
                ( { model | pendingTask = { pending | title = title } }, Cmd.none )

        EditTaskTags newTagsStr ->
            let
                newTags =
                    String.split "," newTagsStr
                        |> List.map String.trim

                pending =
                    model.pendingTask
            in
                ( { model | pendingTask = { pending | tags = newTags } }, Cmd.none )

        EditTaskFrequency frequencyStr ->
            case freqOfString frequencyStr of
                Ok freq ->
                    let
                        pending =
                            model.pendingTask
                    in
                        ( { model | pendingTask = { pending | freq = freq } }, Cmd.none )

                Err str ->
                    -- TODO: real error handling
                    let
                        _ =
                            Debug.log "unable to convert frequency" str
                    in
                        ( model, Cmd.none )

        SaveTask ->
            ( updateModelForApiRequest model
            , saveTask model.pendingTask
            )

        SaveTaskDone succeeded ->
            handleTaskResult model Update succeeded

        AddTask ->
            ( updateModelForApiRequest model
            , addTask model.pendingTask
            )

        AddTaskDone True ->
            let
                ( newModel, cmd ) =
                    handleTaskResult model Create True
            in
                ( { newModel | pendingTask = emptyTask }
                , cmd
                )

        AddTaskDone False ->
            handleTaskResult model Create False


updateModelForApiRequest : Model -> Model
updateModelForApiRequest model =
    { model | apiPending = True, displayResult = Nothing }


type TaskOp
    = Create
    | Read
    | Update
    | Delete


taskOpToInfinitive : TaskOp -> String
taskOpToInfinitive op =
    case op of
        Create ->
            "create"

        Read ->
            "fetch"

        Update ->
            "save"

        Delete ->
            "delete"


taskOpToPastTense : TaskOp -> String
taskOpToPastTense op =
    case op of
        Create ->
            "created"

        Read ->
            "fetched"

        Update ->
            "saved"

        Delete ->
            "deleted"


handleTaskResult : Model -> TaskOp -> Bool -> ( Model, Cmd Msg )
handleTaskResult model op succeeded =
    let
        ( displayResult, nextCmd ) =
            if succeeded then
                ( Just <| Ok <| "Task " ++ (taskOpToPastTense op)
                , fetchTasks ()
                )
            else
                let
                    msg =
                        "failed to " ++ (taskOpToInfinitive op) ++ " task"
                in
                    ( Just (Err msg), Cmd.none )
    in
        ( { model
            | apiPending = False
            , displayResult = displayResult
          }
        , nextCmd
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ authStateChanged Auth.AuthStateChanged |> Sub.map AuthMsg
        , fetchTasksOk (TaskList.FetchTasksDone << decodeTaskListFromValue) |> Sub.map TaskListMsg
        , addTaskOk AddTaskDone
        , deleteTaskOk TaskList.DeleteTaskDone |> Sub.map TaskListMsg
        , saveTaskOk SaveTaskDone
        ]


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


frequencySelect : Frequency -> (String -> Msg) -> Html Msg
frequencySelect selectedFrequency msg =
    let
        opt frequency =
            let
                attrs =
                    if frequency == selectedFrequency then
                        [ selected True ]
                    else
                        []
            in
                option attrs [ text (freqToString frequency) ]
    in
        List.map opt [ Daily, Weekly, Monthly ]
            |> select [ class "form-control", onInput msg ]


showAlert : Maybe DisplayResult -> Html Msg
showAlert displayResult =
    case displayResult of
        Just (Ok msg) ->
            div [ class "alert alert-success" ]
                [ strong [] [ text "Ok! " ]
                , text msg
                ]

        Just (Err msg) ->
            -- TODO: dispaly actual error?
            div [ class "alert alert-danger" ]
                [ strong [] [ text "Error! " ]
                , text msg
                ]

        Nothing ->
            div [] []


maxInputLength : Int
maxInputLength =
    256


editTaskForm : Msg -> Task -> Bool -> Html Msg
editTaskForm submitMsg task apiPending =
    Bootstrap.Forms.form
        FormDefault
        [ onSubmit submitMsg ]
        [ formGroup FormGroupDefault
            [ formLabel [ for "taskTitle" ] [ text "Title" ]
            , formInput
                [ id "taskTitle"
                , value task.title
                , maxlength maxInputLength
                , onInput EditTaskTitle
                ]
                []
            ]
        , formGroup FormGroupDefault
            [ formLabel [ for "taskTags" ] [ text "Tags" ]
            , formInput
                [ id "taskTags"
                , value (String.join ", " task.tags)
                , maxlength maxInputLength
                , onInput EditTaskTags
                ]
                []
            ]
        , formGroup FormGroupDefault
            [ formLabel [ for "taskFrequency" ] [ text "Frequency" ]
            , frequencySelect task.freq EditTaskFrequency
            ]
        , if apiPending then
            btn BtnDefault
                []
                []
                [ type_ "submit", disabled True ]
                [ text "Save Task ", i [ class "fa fa-spinner fa-spin" ] [] ]
          else
            btn BtnDefault [] [] [ type_ "submit" ] [ text "Save Task" ]
        ]


emptyDiv : Html Msg
emptyDiv =
    div [] []


containerWithAlerts : Maybe DisplayResult -> Html Msg -> Html Msg
containerWithAlerts displayResult contents =
    container
        [ row [ showAlert displayResult ]
        , row [ contents ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        HomeRoute ->
            emptyDiv

        TasksRoute ->
            TaskList.viewTasks model.taskListModel |> Html.map TaskListMsg

        TaskAddRoute ->
            editTaskForm AddTask model.pendingTask model.apiPending

        TaskEditRoute taskId ->
            case TaskList.findTaskById taskId model.taskListModel of
                Just _ ->
                    editTaskForm SaveTask model.pendingTask model.apiPending

                Nothing ->
                    -- TODO: return real error here
                    text ("Error: No task with id: " ++ taskId)

        NotFoundRoute ->
            -- TODO: Display 404
            emptyDiv


view : Model -> Html Msg
view model =
    div []
        [ myNavbar model.route model.authModel
        , containerWithAlerts model.displayResult (page model)
        ]

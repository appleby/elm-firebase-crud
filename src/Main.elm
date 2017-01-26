module Main exposing (..)

import Bootstrap.Buttons exposing (..)
import Bootstrap.Forms exposing (..)
import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Navigation exposing (Location)
import String
import UrlParser exposing ((</>))


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initRoute =
            parseLocation location
    in
        ( initModel initRoute, fetchTasks )


initModel : Route -> Model
initModel route =
    { tasks = []
    , route = route
    , apiPending = False
    , displayResult = Nothing
    , pendingTask = emptyTask
    }


emptyTask : Task
emptyTask =
    { id = "", title = "", tags = [], freq = Daily }


type Frequency
    = Daily
    | Weekly
    | Monthly


type alias TaskId =
    String


type alias PostResponse =
    { name : TaskId }


type alias Task =
    { id : TaskId
    , title : String
    , tags : List String
    , freq : Frequency
    }


type alias DisplayResult =
    Result String String


type alias Model =
    { tasks : List Task
    , route : Route
    , apiPending : Bool
    , displayResult : Maybe DisplayResult
    , pendingTask : Task
    }


type Route
    = HomeRoute
    | TasksRoute
    | TaskEditRoute TaskId
    | TaskAddRoute
    | NotFoundRoute


routeToString : Route -> String
routeToString route =
    case route of
        HomeRoute ->
            "#"

        TasksRoute ->
            "#tasks"

        TaskAddRoute ->
            "#tasks/add"

        TaskEditRoute id ->
            "#tasks/" ++ id ++ "/edit"

        NotFoundRoute ->
            "#notfound"


findTaskById : TaskId -> List Task -> Maybe Task
findTaskById id =
    List.Extra.find (\t -> t.id == id)


resetPageState : Model -> Route -> Model
resetPageState model newRoute =
    let
        pending =
            case newRoute of
                TaskEditRoute taskId ->
                    findTaskById taskId model.tasks
                        |> Maybe.withDefault emptyTask

                _ ->
                    emptyTask
    in
        { model
            | pendingTask = pending
            , apiPending = False
            , displayResult = Nothing
        }


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map TaskAddRoute (UrlParser.s "tasks" </> UrlParser.s "add")
        , UrlParser.map TaskEditRoute (UrlParser.s "tasks" </> UrlParser.string </> UrlParser.s "edit")
        , UrlParser.map TasksRoute (UrlParser.s "tasks")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


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


dbUrl : String
dbUrl =
    "https://timeslots-61887.firebaseio.com/test/"


addTaskUrl : String
addTaskUrl =
    dbUrl ++ "tasks.json"


deleteTaskUrl : TaskId -> String
deleteTaskUrl id =
    dbUrl ++ "tasks/" ++ id ++ ".json"


fetchTasksUrl : String
fetchTasksUrl =
    addTaskUrl


saveTaskUrl : TaskId -> String
saveTaskUrl =
    deleteTaskUrl


addTask : Task -> Cmd Msg
addTask task =
    Http.request
        { body = encodeTask task |> Http.jsonBody
        , expect = Http.expectJson (addResponseDecoder task)
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = addTaskUrl
        , withCredentials = False
        }
        |> Http.send AddTaskDone


deleteTask : Task -> Cmd Msg
deleteTask task =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson <| Json.Decode.succeed task
        , headers = []
        , method = "DELETE"
        , timeout = Nothing
        , url = deleteTaskUrl task.id
        , withCredentials = False
        }
        |> Http.send DeleteTaskDone


fetchTasks : Cmd Msg
fetchTasks =
    Http.get fetchTasksUrl taskListDecoder
        |> Http.send FetchTasksDone


saveTask : Task -> Cmd Msg
saveTask task =
    Http.request
        { body = encodeTask task |> Http.jsonBody
        , expect = Http.expectJson taskDecoder
        , headers = []
        , method = "PUT"
        , timeout = Nothing
        , url = saveTaskUrl task.id
        , withCredentials = False
        }
        |> Http.send SaveTaskDone


encodeTask : Task -> Json.Encode.Value
encodeTask task =
    Json.Encode.object
        [ ( "title", Json.Encode.string task.title )
        , ( "tags", List.map Json.Encode.string task.tags |> Json.Encode.list )
        , ( "frequency", Json.Encode.string (freqToString task.freq) )
        ]


taskListDecoder : Json.Decode.Decoder (List Task)
taskListDecoder =
    Json.Decode.keyValuePairs taskDecoder
        |> Json.Decode.andThen
            (Json.Decode.succeed
                << List.map (\( id, task ) -> { task | id = id })
            )


taskDecoder : Json.Decode.Decoder Task
taskDecoder =
    Json.Decode.map4 Task
        (Json.Decode.succeed "")
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "tags" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "frequency" Json.Decode.string |> Json.Decode.andThen freqDecoder)


postResponseDecoder : Json.Decode.Decoder PostResponse
postResponseDecoder =
    Json.Decode.map PostResponse (Json.Decode.field "name" Json.Decode.string)


addResponseDecoder : Task -> Json.Decode.Decoder Task
addResponseDecoder task =
    postResponseDecoder
        |> Json.Decode.andThen
            (\pr -> Json.Decode.succeed { task | id = pr.name })


freqDecoder : String -> Json.Decode.Decoder Frequency
freqDecoder str =
    case freqOfString str of
        Ok freq ->
            Json.Decode.succeed freq

        Err msg ->
            Json.Decode.fail ("unable to decode frequency: " ++ msg)


type alias ApiTaskResult =
    Result Http.Error Task


type Msg
    = OnLocationChange Location
    | Goto Route
    | FetchTasksDone (Result Http.Error (List Task))
    | EditTaskTitle String
    | EditTaskTags String
    | EditTaskFrequency String
    | SaveTask
    | SaveTaskDone ApiTaskResult
    | AddTask
    | AddTaskDone ApiTaskResult
    | DeleteTask Task
    | DeleteTaskDone ApiTaskResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto newRoute ->
            ( model, Navigation.newUrl (routeToString newRoute) )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                resetModel =
                    resetPageState model newRoute
            in
                ( { resetModel | route = newRoute }, Cmd.none )

        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "failed to fetch tasks" error
            in
                ( model, Cmd.none )

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
            ( { model | apiPending = True, displayResult = Nothing }
            , saveTask model.pendingTask
            )

        SaveTaskDone apiResult ->
            handleApiTaskResult model Update apiResult

        AddTask ->
            ( { model | apiPending = True, displayResult = Nothing }
            , addTask model.pendingTask
            )

        AddTaskDone ((Ok _) as apiResult) ->
            let
                ( newModel, cmd ) =
                    handleApiTaskResult model Create apiResult
            in
                ( { newModel | pendingTask = emptyTask }
                , cmd
                )

        AddTaskDone ((Err _) as apiResult) ->
            handleApiTaskResult model Create apiResult

        DeleteTask task ->
            ( { model | apiPending = True, displayResult = Nothing }
            , deleteTask task
            )

        DeleteTaskDone apiResult ->
            handleApiTaskResult model Delete apiResult


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


handleApiTaskResult : Model -> TaskOp -> ApiTaskResult -> ( Model, Cmd Msg )
handleApiTaskResult model op apiResult =
    let
        ( displayResult, nextCmd ) =
            case apiResult of
                Ok _ ->
                    ( Just <| Ok <| "Task " ++ (taskOpToPastTense op)
                    , fetchTasks
                    )

                Err error ->
                    let
                        msg =
                            "failed to " ++ (taskOpToInfinitive op) ++ " task"

                        _ =
                            Debug.log msg error
                    in
                        ( Just (Err msg), Cmd.none )
    in
        ( { model
            | apiPending = False
            , displayResult = displayResult
          }
        , nextCmd
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
                    [ onClick (Goto HomeRoute) ]
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
                    , navLink currentRoute TaskAddRoute "Add Task"
                    ]
                ]
            ]
        ]


viewTask : Task -> Html Msg
viewTask task =
    tr []
        [ td [] [ text task.title ]
        , td [] [ text (freqToString task.freq) ]
        , td [] [ text (String.join ", " task.tags) ]
        , td []
            [ div [ class "btn-group" ]
                [ btn BtnDefault
                    []
                    []
                    [ onClick (Goto <| TaskEditRoute task.id) ]
                    [ text "edit" ]
                , btn BtnDefault
                    []
                    []
                    [ onClick (DeleteTask task) ]
                    [ text "delete" ]
                ]
            ]
        ]


viewTasks : List Task -> Html Msg
viewTasks tasks =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Title" ]
                , th [] [ text "Frequency" ]
                , th [] [ text "Tags" ]
                , th [] [ text "Action" ]
                ]
            ]
        , tbody [] (List.map viewTask tasks)
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
            viewTasks model.tasks

        TaskAddRoute ->
            editTaskForm AddTask model.pendingTask model.apiPending

        TaskEditRoute taskId ->
            case findTaskById taskId model.tasks of
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
        [ myNavbar model.route
        , containerWithAlerts model.displayResult (page model)
        ]

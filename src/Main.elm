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


findTaskById : TaskId -> List Task -> Maybe Task
findTaskById id =
    List.Extra.find (\t -> t.id == id)


type alias Model =
    { tasks : List Task
    , route : Route
    , savePending : Bool
    , saveSuccess : Maybe Bool
    , addTask : Task
    }


resetPageState : Model -> Model
resetPageState model =
    { model | addTask = emptyTask, savePending = False, saveSuccess = Nothing }


type Route
    = HomeRoute
    | TasksRoute
    | TaskRoute TaskId
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

        TaskRoute id ->
            "#tasks/" ++ (toString id)

        TaskAddRoute ->
            "#tasks/add"

        TaskEditRoute id ->
            "#tasks/" ++ (toString id) ++ "/edit"

        NotFoundRoute ->
            "#notfound"


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map TaskAddRoute (UrlParser.s "tasks" </> UrlParser.s "add")
        , UrlParser.map TaskEditRoute (UrlParser.s "tasks" </> UrlParser.int </> UrlParser.s "edit")
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


emptyTask : Task
emptyTask =
    { id = 0, title = "", tags = [], freq = Daily }


initModel : Route -> Model
initModel route =
    { tasks = []
    , route = route
    , savePending = False
    , saveSuccess = Nothing
    , addTask = emptyTask
    }


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


saveTaskUrl : TaskId -> String
saveTaskUrl id =
    "http://localhost:4000/tasks/" ++ (toString id)


fetchTasksUrl : String
fetchTasksUrl =
    "http://localhost:4000/tasks"


addTaskUrl : String
addTaskUrl =
    fetchTasksUrl


fetchTasks : Cmd Msg
fetchTasks =
    Http.get fetchTasksUrl (Json.Decode.list taskDecoder)
        |> Http.send FetchTasksDone


commonTaskFields : Task -> List ( String, Json.Encode.Value )
commonTaskFields task =
    [ ( "title", Json.Encode.string task.title )
    , ( "tags", List.map Json.Encode.string task.tags |> Json.Encode.list )
    , ( "frequency", Json.Encode.string (freqToString task.freq) )
    ]


encodeTask : Task -> Json.Encode.Value
encodeTask task =
    Json.Encode.object <|
        ( "id", Json.Encode.int task.id )
            :: (commonTaskFields task)


encodeNewTask : Task -> Json.Encode.Value
encodeNewTask task =
    Json.Encode.object (commonTaskFields task)


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
    | Goto Route
    | FetchTasksDone (Result Http.Error (List Task))
    | EditTaskTitle TaskId String
    | EditTaskTags TaskId String
    | EditTaskFrequency TaskId String
    | SaveTask TaskId
    | SaveTaskDone (Result Http.Error Task)
    | AddTaskTitle String
    | AddTaskTags String
    | AddTaskFrequency String
    | AddTask
    | AddTaskDone (Result Http.Error Task)


saveTask : Task -> Cmd Msg
saveTask task =
    Http.request
        { body = encodeTask task |> Http.jsonBody
        , expect = Http.expectJson taskDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = saveTaskUrl task.id
        , withCredentials = False
        }
        |> Http.send SaveTaskDone


addTask : Task -> Cmd Msg
addTask task =
    Http.request
        { body = encodeNewTask task |> Http.jsonBody
        , expect = Http.expectJson taskDecoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = addTaskUrl
        , withCredentials = False
        }
        |> Http.send AddTaskDone


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
                    resetPageState model
            in
                ( { resetModel | route = newRoute }, Cmd.none )

        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "Fetch error" error
            in
                ( model, Cmd.none )

        EditTaskTitle taskId newTitle ->
            let
                newTasks =
                    List.map
                        (\t ->
                            if t.id == taskId then
                                { t | title = newTitle }
                            else
                                t
                        )
                        model.tasks
            in
                ( { model | tasks = newTasks }, Cmd.none )

        EditTaskTags taskId newTagsStr ->
            let
                newTags =
                    String.split "," newTagsStr
                        |> List.map String.trim

                newTasks =
                    List.map
                        (\t ->
                            if t.id == taskId then
                                { t | tags = newTags }
                            else
                                t
                        )
                        model.tasks
            in
                ( { model | tasks = newTasks }, Cmd.none )

        EditTaskFrequency taskId newFrequencyStr ->
            case freqOfString newFrequencyStr of
                Ok newFrequency ->
                    let
                        newTasks =
                            List.map
                                (\t ->
                                    if t.id == taskId then
                                        { t | freq = newFrequency }
                                    else
                                        t
                                )
                                model.tasks
                    in
                        ( { model | tasks = newTasks }, Cmd.none )

                Err str ->
                    -- TODO: real error handling
                    let
                        _ =
                            Debug.log str
                    in
                        ( model, Cmd.none )

        SaveTask taskId ->
            case findTaskById taskId model.tasks of
                Just task ->
                    ( { model | savePending = True, saveSuccess = Nothing }
                    , saveTask task
                    )

                Nothing ->
                    -- TODO: real error handling
                    let
                        _ =
                            Debug.log <| "SaveTask: invalid taskId " ++ (toString taskId)
                    in
                        ( model, Cmd.none )

        SaveTaskDone (Ok _) ->
            ( { model | savePending = False, saveSuccess = Just True }
            , Cmd.none
            )

        SaveTaskDone (Err error) ->
            -- TODO: real error handling
            -- TODO: only fetch the required task?
            ( { model | savePending = False, saveSuccess = Just False }
            , fetchTasks
            )

        AddTaskTitle title ->
            let
                addTask =
                    model.addTask
            in
                ( { model | addTask = { addTask | title = title } }, Cmd.none )

        AddTaskTags newTagsStr ->
            let
                newTags =
                    String.split "," newTagsStr
                        |> List.map String.trim

                addTask =
                    model.addTask
            in
                ( { model | addTask = { addTask | tags = newTags } }, Cmd.none )

        AddTaskFrequency frequencyStr ->
            case freqOfString frequencyStr of
                Ok freq ->
                    let
                        addTask =
                            model.addTask
                    in
                        ( { model | addTask = { addTask | freq = freq } }, Cmd.none )

                Err str ->
                    -- TODO: real error handling
                    let
                        _ =
                            Debug.log str
                    in
                        ( model, Cmd.none )

        AddTask ->
            ( { model | savePending = True, saveSuccess = Nothing }
            , addTask model.addTask
            )

        AddTaskDone (Ok task) ->
            ( { model
                | tasks = task :: model.tasks
                , addTask = emptyTask
                , savePending = False
                , saveSuccess = Just True
              }
            , Cmd.none
            )

        AddTaskDone (Err error) ->
            -- TODO: real error handling
            ( { model | savePending = False, saveSuccess = Just False }
            , Cmd.none
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
        [ td [] [ text (toString task.id) ]
        , td [] [ text task.title ]
        , td [] [ text (freqToString task.freq) ]
        , td [] [ text (String.join ", " task.tags) ]
        , td []
            [ div [ class "btn-group" ]
                [ btn BtnDefault [] [] [] [ text "view" ]
                , btn BtnDefault
                    []
                    []
                    [ onClick (Goto <| TaskEditRoute task.id) ]
                    [ text "edit" ]
                , btn BtnDefault [] [] [] [ text "delete" ]
                ]
            ]
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


saveTaskAlert : Maybe Bool -> Html Msg
saveTaskAlert saveSuccess =
    case saveSuccess of
        Just success ->
            if success then
                div [ class "alert alert-success" ]
                    [ strong [] [ text "Ok! " ]
                    , text "Task saved"
                    ]
            else
                -- TODO: dispaly error?
                div [ class "alert alert-danger" ]
                    [ strong [] [ text "Error! " ]
                    , text "Failed to save task!"
                    ]

        Nothing ->
            div [] []


maxInputLength : Int
maxInputLength =
    256


addTaskForm : Task -> Bool -> Maybe Bool -> Html Msg
addTaskForm task savePending saveSuccess =
    container
        [ row [ saveTaskAlert saveSuccess ]
        , row
            [ Bootstrap.Forms.form
                FormDefault
                [ onSubmit AddTask ]
                [ formGroup FormGroupDefault
                    [ formLabel [ for "taskTitle" ] [ text "Title" ]
                    , formInput
                        [ id "taskTitle"
                        , value task.title
                        , maxlength maxInputLength
                        , onInput AddTaskTitle
                        ]
                        []
                    ]
                , formGroup FormGroupDefault
                    [ formLabel [ for "taskTags" ] [ text "Tags" ]
                    , formInput
                        [ id "taskTags"
                        , value (String.join ", " task.tags)
                        , maxlength maxInputLength
                        , onInput AddTaskTags
                        ]
                        []
                    ]
                , formGroup FormGroupDefault
                    [ formLabel [ for "taskFrequency" ] [ text "Frequency" ]
                    , frequencySelect task.freq AddTaskFrequency
                    ]
                , if savePending then
                    btn BtnDefault
                        []
                        []
                        [ type_ "submit", disabled True ]
                        [ text "Save Task ", i [ class "fa fa-spinner fa-spin" ] [] ]
                  else
                    btn BtnDefault [] [] [ type_ "submit" ] [ text "Save Task" ]
                ]
            ]
        ]


editTaskForm : Task -> Bool -> Maybe Bool -> Html Msg
editTaskForm task savePending saveSuccess =
    container
        [ row [ saveTaskAlert saveSuccess ]
        , row
            [ Bootstrap.Forms.form
                FormDefault
                [ onSubmit (SaveTask task.id) ]
                [ formGroup FormGroupDefault
                    [ formLabel [ for "taskTitle" ] [ text "Title" ]
                    , formInput
                        [ id "taskTitle"
                        , value task.title
                        , maxlength maxInputLength
                        , onInput (EditTaskTitle task.id)
                        ]
                        []
                    ]
                , formGroup FormGroupDefault
                    [ formLabel [ for "taskTags" ] [ text "Tags" ]
                    , formInput
                        [ id "taskTags"
                        , value (String.join ", " task.tags)
                        , maxlength maxInputLength
                        , onInput (EditTaskTags task.id)
                        ]
                        []
                    ]
                , formGroup FormGroupDefault
                    [ formLabel [ for "taskFrequency" ] [ text "Frequency" ]
                    , frequencySelect task.freq (EditTaskFrequency task.id)
                    ]
                , if savePending then
                    btn BtnDefault
                        []
                        []
                        [ type_ "submit", disabled True ]
                        [ text "Save Task ", i [ class "fa fa-spinner fa-spin" ] [] ]
                  else
                    btn BtnDefault [] [] [ type_ "submit" ] [ text "Save Task" ]
                ]
            ]
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

        TaskAddRoute ->
            addTaskForm model.addTask model.savePending model.saveSuccess

        TaskEditRoute taskId ->
            case findTaskById taskId model.tasks of
                Just task ->
                    editTaskForm task model.savePending model.saveSuccess

                Nothing ->
                    -- TODO: return real error here
                    container [ row [ text <| "Error: No task with id: " ++ (toString taskId) ] ]

        NotFoundRoute ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ myNavbar model.route
        , page model
        ]

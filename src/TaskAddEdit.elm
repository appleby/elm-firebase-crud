module TaskAddEdit
    exposing
        ( Model
          -- TODO: Don't expose Msg constructors
        , Msg(AddTask, SaveTask)
        , subscriptions
        , update
        , view
        , authRequired
        , initModel
        , mountEditCmd
        )

import Bootstrap.Buttons exposing (ButtonOption(..), btn)
import Bootstrap.Forms
    exposing
        ( FormAlignmentOption(..)
        , FormGroupOption(..)
        , formGroup
        , formInput
        , formLabel
        )
import Bootstrap.Grid exposing (container, row)
import Data exposing (..)
import Debug
import Html exposing (Html, div, i, option, select, strong, text)
import Html.Attributes exposing (disabled, class, for, id, maxlength, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Ports exposing (..)
import String


type alias DisplayResult =
    Result String String


type alias Model =
    { apiPending : Bool
    , displayResult : Maybe DisplayResult
    , pendingTask : Task
    }


initModel : Model
initModel =
    Model False Nothing emptyTask


type Msg
    = EditTaskTitle String
    | EditTaskTags String
    | EditTaskFrequency String
    | FetchTaskDone (Result String Task)
    | SaveTask
    | SaveTaskDone Bool
    | AddTask
    | AddTaskDone Bool


mountEditCmd : TaskId -> Cmd Msg
mountEditCmd taskId =
    fetchTask taskId


authRequired : Msg -> Bool
authRequired _ =
    True


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fetchTaskOk (FetchTaskDone << decodeTaskFromValue)
        , addTaskOk AddTaskDone
        , saveTaskOk SaveTaskDone
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTaskDone (Ok task) ->
            ( { model | pendingTask = task }, Cmd.none )

        FetchTaskDone (Err error) ->
            let
                _ =
                    Debug.log "failed to fetch task" error
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


editTaskForm : Msg -> Model -> Html Msg
editTaskForm submitMsg model =
    Bootstrap.Forms.form
        FormDefault
        [ onSubmit submitMsg ]
        [ formGroup FormGroupDefault
            [ formLabel [ for "taskTitle" ] [ text "Title" ]
            , formInput
                [ id "taskTitle"
                , value model.pendingTask.title
                , maxlength maxInputLength
                , onInput EditTaskTitle
                ]
                []
            ]
        , formGroup FormGroupDefault
            [ formLabel [ for "taskTags" ] [ text "Tags" ]
            , formInput
                [ id "taskTags"
                , value (String.join ", " model.pendingTask.tags)
                , maxlength maxInputLength
                , onInput EditTaskTags
                ]
                []
            ]
        , formGroup FormGroupDefault
            [ formLabel [ for "taskFrequency" ] [ text "Frequency" ]
            , frequencySelect model.pendingTask.freq EditTaskFrequency
            ]
        , if model.apiPending then
            btn BtnDefault
                []
                []
                [ type_ "submit", disabled True ]
                [ text "Save Task ", i [ class "fa fa-spinner fa-spin" ] [] ]
          else
            btn BtnDefault [] [] [ type_ "submit" ] [ text "Save Task" ]
        ]


containerWithAlerts : Maybe DisplayResult -> Html Msg -> Html Msg
containerWithAlerts displayResult contents =
    container
        [ row [ showAlert displayResult ]
        , row [ contents ]
        ]


view : Msg -> Model -> Html Msg
view submitMsg model =
    containerWithAlerts model.displayResult (editTaskForm submitMsg model)

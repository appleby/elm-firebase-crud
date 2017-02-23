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
        , mountAdd
        , mountEdit
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
import Data exposing (..)
import Debug
import DisplayResult exposing (containerWithAlerts)
import Html exposing (Html, div, i, option, select, strong, text)
import Html.Attributes exposing (disabled, class, for, id, maxlength, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Ports exposing (..)
import String
import TaskOp exposing (TaskOper, updateModelForApiRequest)


type alias Model =
    TaskOper { pendingTask : Task }


initModel : Model
initModel =
    { apiPending = False
    , displayResult = Nothing
    , pendingTask = emptyTask
    }


type Msg
    = EditTaskTitle String
    | EditTaskTags String
    | EditTaskFrequency String
    | FetchTaskDone (Result String Task)
    | SaveTask
    | SaveTaskDone Bool
    | AddTask
    | AddTaskDone Bool


mountAdd : Model -> ( Model, Cmd Msg )
mountAdd model =
    ( initModel, Cmd.none )


mountEdit : Model -> TaskId -> ( Model, Cmd Msg )
mountEdit model taskId =
    ( initModel, fetchTask taskId )


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
            TaskOp.handleResult model TaskOp.Update succeeded Cmd.none Cmd.none

        AddTask ->
            ( updateModelForApiRequest model
            , addTask model.pendingTask
            )

        AddTaskDone True ->
            let
                ( newModel, cmd ) =
                    TaskOp.handleResult model TaskOp.Create True Cmd.none Cmd.none
            in
                ( { newModel | pendingTask = emptyTask }
                , cmd
                )

        AddTaskDone False ->
            TaskOp.handleResult model TaskOp.Create False Cmd.none Cmd.none


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


view : Msg -> Model -> Html Msg
view submitMsg model =
    containerWithAlerts model (editTaskForm submitMsg model)

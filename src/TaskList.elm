module TaskList
    exposing
        ( Model
        , Msg
        , subscriptions
        , update
        , view
        , authRequired
        , initModel
        , mount
        )

import Bootstrap.Buttons exposing (ButtonOption(..), btn)
import Data exposing (Task, freqToString)
import DisplayResult exposing (containerWithAlerts)
import Html exposing (Html, div, table, thead, tbody, td, th, tr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ports
import Route exposing (Route(..))
import TaskOp exposing (TaskOper, updateModelForApiRequest)


type alias Model =
    TaskOper { tasks : List Task }


type Msg
    = DeleteTask Task
    | DeleteTaskDone Bool
    | FetchTasksDone (Result String (List Task))


initModel : Model
initModel =
    { apiPending = False, displayResult = Nothing, tasks = [] }


mount : Model -> ( Model, Cmd Msg )
mount model =
    ( { initModel | tasks = model.tasks }, Ports.fetchTasks () )


authRequired : Msg -> Bool
authRequired _ =
    True


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.fetchTasksOk (FetchTasksDone << Ports.decodeTaskList)
        , Ports.deleteTaskOk DeleteTaskDone
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "failed to fetch tasks" error
            in
                TaskOp.handleResult model TaskOp.Read False Cmd.none Cmd.none

        DeleteTask task ->
            ( model, Ports.deleteTask task.id )

        DeleteTaskDone succeeded ->
            TaskOp.handleResult model
                TaskOp.Delete
                succeeded
                (Ports.fetchTasks ())
                Cmd.none


viewTask : Task -> Html Msg
viewTask task =
    tr []
        [ td [] [ text task.title ]
        , td [] [ text (freqToString task.freq) ]
        , td [] [ text (String.join ", " task.tags) ]
        , td []
            [ div [ class "btn-group" ]
                [ Route.linkTo
                    (TaskEditRoute task.id)
                    [ class "btn btn-default" ]
                    [ text "edit" ]
                , btn BtnDefault
                    []
                    []
                    [ onClick (DeleteTask task) ]
                    [ text "delete" ]
                ]
            ]
        ]


viewTasks : Model -> Html Msg
viewTasks model =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Title" ]
                , th [] [ text "Frequency" ]
                , th [] [ text "Tags" ]
                , th [] [ text "Action" ]
                ]
            ]
        , tbody [] (List.map viewTask model.tasks)
        ]


view : Model -> Html Msg
view model =
    containerWithAlerts model (viewTasks model)

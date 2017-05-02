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
        , unmount
        )

import Bootstrap.Buttons exposing (ButtonOption(..), btn)
import Data exposing (Task, freqToString)
import DisplayResult exposing (containerWithAlerts)
import Html exposing (Html, div, table, thead, td, th, tr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed
import Ports
import Route exposing (Route(..))
import TaskOp exposing (TaskOper)


type alias Model =
    TaskOper { tasks : List Task }


type Msg
    = DeleteTask Task
    | DeleteTaskDone Bool
    | GotUserTasks (Result String (List Task))


initModel : Model
initModel =
    { apiPending = False, displayResult = Nothing, tasks = [] }


mount : Model -> ( Model, Cmd Msg )
mount model =
    ( initModel, Ports.subscribeToUserTasks () )


unmount : Model -> ( Model, Cmd Msg )
unmount model =
    ( initModel, Ports.unSubscribeFromUserTasks () )


authRequired : Msg -> Bool
authRequired _ =
    True


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.userTasksOk (GotUserTasks << Ports.decodeTaskList)
        , Ports.deleteTaskOk DeleteTaskDone
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUserTasks (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        GotUserTasks (Err error) ->
            let
                _ =
                    Debug.log "failed to decode tasks" error
            in
                ( TaskOp.handleResult model TaskOp.Read False, Cmd.none )

        DeleteTask task ->
            ( model, Ports.deleteTask task.id )

        DeleteTaskDone succeeded ->
            ( TaskOp.handleResult model TaskOp.Delete succeeded, Cmd.none )


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


viewKeyedTask : Task -> ( String, Html Msg )
viewKeyedTask task =
    ( task.id, viewTask task )


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
        , Html.Keyed.node "tbody" [] (List.map viewKeyedTask model.tasks)
        ]


view : Model -> Html Msg
view model =
    containerWithAlerts model (viewTasks model)

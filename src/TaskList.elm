module TaskList
    exposing
        ( Model
        , Msg
        , subscriptions
        , update
        , viewTasks
        , authRequired
        , initModel
        , mount
        )

import Bootstrap.Buttons exposing (ButtonOption(..), btn)
import Data exposing (Task, freqToString)
import Html exposing (Html, div, table, thead, tbody, td, th, tr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Ports
import Route exposing (Route(..))


type alias Model =
    { tasks : List Task
    }


type Msg
    = DeleteTask Task
    | DeleteTaskDone Bool
    | FetchTasksDone (Result String (List Task))
    | Goto Route


initModel : Model
initModel =
    Model []


mount : Cmd Msg
mount =
    Ports.fetchTasks ()


authRequired : Msg -> Bool
authRequired _ =
    True


subscriptions _ =
    Sub.batch
        [ Ports.fetchTasksOk (FetchTasksDone << Ports.decodeTaskListFromValue)
        , Ports.deleteTaskOk DeleteTaskDone
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto newRoute ->
            ( model, Route.goto newRoute )

        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "failed to fetch tasks" error
            in
                ( model, Cmd.none )

        DeleteTask task ->
            ( model, Ports.deleteTask task.id )

        DeleteTaskDone _ ->
            -- TODO: display succ / fail
            ( model, Ports.fetchTasks () )


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

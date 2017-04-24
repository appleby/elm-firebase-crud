port module Ports exposing (..)

import Data exposing (Frequency, User, Task, TaskId, freqToString, freqOfString)
import Json.Decode as JD
import Json.Encode as JE


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port authStateChanged : (Maybe User -> msg) -> Sub msg


port fetchTasks : () -> Cmd msg


port fetchTask : TaskId -> Cmd msg


port addTaskPort : JE.Value -> Cmd msg


port deleteTask : TaskId -> Cmd msg


port saveTaskPort : JE.Value -> Cmd msg


port fetchTasksOk : (JD.Value -> msg) -> Sub msg


port fetchTaskOk : (JD.Value -> msg) -> Sub msg


port addTaskOk : (Bool -> msg) -> Sub msg


port deleteTaskOk : (Bool -> msg) -> Sub msg


port saveTaskOk : (Bool -> msg) -> Sub msg


addTask : Task -> Cmd msg
addTask task =
    addTaskPort (encodeNewTask task)


saveTask : Task -> Cmd msg
saveTask task =
    saveTaskPort (encodeTask task)


encodeNewTask : Task -> JE.Value
encodeNewTask task =
    JE.object
        [ ( "title", JE.string task.title )
        , ( "tags", List.map JE.string task.tags |> JE.list )
        , ( "frequency", JE.string (freqToString task.freq) )
        ]


encodeTask : Task -> JE.Value
encodeTask task =
    JE.object
        [ ( "id", JE.string task.id )
        , ( "title", JE.string task.title )
        , ( "tags", List.map JE.string task.tags |> JE.list )
        , ( "frequency", JE.string (freqToString task.freq) )
        ]


taskListDecoder : JD.Decoder (List Task)
taskListDecoder =
    -- Ignore the keys, since every Task contains a copy at task.id.
    JD.keyValuePairs taskDecoder
        |> JD.map (List.map Tuple.second)


taskDecoder : JD.Decoder Task
taskDecoder =
    JD.map4 Task
        (JD.field "id" JD.string)
        (JD.field "title" JD.string)
        (JD.field "tags" (JD.list JD.string))
        (JD.field "frequency" JD.string |> JD.andThen freqDecoder)


freqDecoder : String -> JD.Decoder Frequency
freqDecoder str =
    case freqOfString str of
        Ok freq ->
            JD.succeed freq

        Err msg ->
            JD.fail ("unable to decode frequency: " ++ msg)


decodeTaskList : JD.Value -> Result String (List Task)
decodeTaskList =
    JD.decodeValue taskListDecoder


decodeNullableTask : JD.Value -> Result String Task
decodeNullableTask jsonValue =
    case JD.decodeValue (JD.nullable taskDecoder) jsonValue of
        Ok maybeTask ->
            Result.fromMaybe "No such task" maybeTask

        Err msg ->
            Err msg

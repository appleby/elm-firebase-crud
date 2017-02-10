port module Ports exposing (..)

import Data exposing (..)
import Json.Decode
import Json.Encode


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port authStateChanged : (Maybe User -> msg) -> Sub msg


port fetchTasks : () -> Cmd msg


port addTaskPort : Json.Encode.Value -> Cmd msg


port deleteTask : TaskId -> Cmd msg


port saveTaskPort : Json.Encode.Value -> Cmd msg


port fetchTasksOk : (Json.Decode.Value -> msg) -> Sub msg


port addTaskOk : (Bool -> msg) -> Sub msg


port deleteTaskOk : (Bool -> msg) -> Sub msg


port saveTaskOk : (Bool -> msg) -> Sub msg


addTask : Task -> Cmd msg
addTask task =
    addTaskPort (encodeTask task)


saveTask : Task -> Cmd msg
saveTask task =
    saveTaskPort (encodeTaskWithId task)


encodeTask : Task -> Json.Encode.Value
encodeTask task =
    Json.Encode.object
        [ ( "title", Json.Encode.string task.title )
        , ( "tags", List.map Json.Encode.string task.tags |> Json.Encode.list )
        , ( "frequency", Json.Encode.string (freqToString task.freq) )
        ]


encodeTaskWithId : Task -> Json.Encode.Value
encodeTaskWithId task =
    Json.Encode.object
        [ ( "id", Json.Encode.string task.id )
        , ( "title", Json.Encode.string task.title )
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


freqDecoder : String -> Json.Decode.Decoder Frequency
freqDecoder str =
    case freqOfString str of
        Ok freq ->
            Json.Decode.succeed freq

        Err msg ->
            Json.Decode.fail ("unable to decode frequency: " ++ msg)


decodeTaskListFromValue : Json.Decode.Value -> Result String (List Task)
decodeTaskListFromValue =
    Json.Decode.decodeValue taskListDecoder

port module Ports
    exposing
        ( addTask
        , authStateChanged
        , decodeNullableTask
        , decodeTaskList
        , deleteTask
        , encodeTask
        , encodeNewTask
        , fetchTask
        , fetchTasks
        , saveTask
        , signIn
        , signOut
        , addTaskOk
        , deleteTaskOk
        , fetchTaskOk
        , fetchTasksOk
        , saveTaskOk
        )

import Data exposing (Frequency, User, Task, TaskId, freqToString, freqOfString)
import Json.Decode as JD
import Json.Encode as JE


-- Auth ports


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port authStateChanged : (Maybe User -> msg) -> Sub msg



-- Task ports


port addTaskPort : JE.Value -> Cmd msg


port deleteTask : TaskId -> Cmd msg


port fetchTask : TaskId -> Cmd msg


port fetchTasks : () -> Cmd msg


port saveTaskPort : JE.Value -> Cmd msg


port addTaskOk : (Bool -> msg) -> Sub msg


port deleteTaskOk : (Bool -> msg) -> Sub msg


port fetchTaskOk : (JD.Value -> msg) -> Sub msg


port fetchTasksOk : (JD.Value -> msg) -> Sub msg


port saveTaskOk : (Bool -> msg) -> Sub msg


addTask : Task -> Cmd msg
addTask =
    addTaskPort << encodeNewTask


saveTask : Task -> Cmd msg
saveTask =
    saveTaskPort << encodeTask



-- Encoders


commonTaskFields : Task -> List ( String, JE.Value )
commonTaskFields task =
    [ ( "title", JE.string task.title )
    , ( "tags", List.map JE.string task.tags |> JE.list )
    , ( "frequency", JE.string (freqToString task.freq) )
    ]


encodeNewTask : Task -> JE.Value
encodeNewTask =
    -- New tasks don't yet have a task.id.
    JE.object << commonTaskFields


encodeTask : Task -> JE.Value
encodeTask task =
    JE.object <| ( "id", JE.string task.id ) :: (commonTaskFields task)



-- Decoders


decodeTaskList : JD.Value -> Result String (List Task)
decodeTaskList =
    JD.decodeValue (JD.nullable taskListDecoder)
        >> Result.map (Maybe.withDefault [])


decodeNullableTask : JD.Value -> Result String Task
decodeNullableTask jsonValue =
    case JD.decodeValue (JD.nullable taskDecoder) jsonValue of
        Ok maybeTask ->
            Result.fromMaybe "No such task" maybeTask

        Err msg ->
            Err msg


freqDecoder : String -> JD.Decoder Frequency
freqDecoder str =
    case freqOfString str of
        Ok freq ->
            JD.succeed freq

        Err msg ->
            JD.fail ("unable to decode frequency: " ++ msg)


taskDecoder : JD.Decoder Task
taskDecoder =
    JD.map4 Task
        (JD.field "id" JD.string)
        (JD.field "title" JD.string)
        (JD.maybe (JD.field "tags" (JD.list JD.string))
            |> JD.andThen (JD.succeed << Maybe.withDefault [])
        )
        (JD.field "frequency" JD.string |> JD.andThen freqDecoder)


taskListDecoder : JD.Decoder (List Task)
taskListDecoder =
    -- Ignore the keys, since every Task contains a copy at task.id.
    JD.keyValuePairs taskDecoder
        |> JD.map (List.map Tuple.second)

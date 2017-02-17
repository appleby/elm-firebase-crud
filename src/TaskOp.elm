module TaskOp
    exposing
        ( TaskOp(..)
        , TaskOper
        , updateModelForApiRequest
        , handleTaskResult
        )

import DisplayResult exposing (DisplayResult)


type TaskOp
    = Create
    | Read
    | Update
    | Delete


type alias TaskOper a =
    { a | apiPending : Bool, displayResult : Maybe DisplayResult }


updateModelForApiRequest : TaskOper a -> TaskOper a
updateModelForApiRequest model =
    { model | apiPending = True, displayResult = Nothing }


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


handleTaskResult :
    TaskOper a
    -> TaskOp
    -> Bool
    -> Cmd msg
    -> Cmd msg
    -> ( TaskOper a, Cmd msg )
handleTaskResult model op succeeded succCmd failCmd =
    let
        ( displayResult, nextCmd ) =
            if succeeded then
                ( Just <| Ok <| "Task " ++ (taskOpToPastTense op)
                , succCmd
                )
            else
                let
                    msg =
                        "failed to " ++ (taskOpToInfinitive op) ++ " task"
                in
                    ( Just (Err msg), failCmd )
    in
        ( { model
            | apiPending = False
            , displayResult = displayResult
          }
        , nextCmd
        )

module TaskOp
    exposing
        ( TaskOp(..)
        , TaskOper
        , start
        , complete
        )

import DisplayResult exposing (ResultDisplayer)


type TaskOp
    = Create
    | Read
    | Update
    | Delete


type alias TaskOper a =
    ResultDisplayer { a | apiPending : Bool }


start : TaskOper a -> TaskOper a
start model =
    DisplayResult.nothing { model | apiPending = True }


complete :
    TaskOp
    -> Bool
    -> TaskOper a
    -> TaskOper a
complete op succeeded model =
    if succeeded then
        DisplayResult.succ (succMessage op) { model | apiPending = False }
    else
        DisplayResult.fail (failMessage op) { model | apiPending = False }


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


succMessage : TaskOp -> String
succMessage op =
    "Task " ++ (taskOpToPastTense op)


failMessage : TaskOp -> String
failMessage op =
    "Failed to " ++ (taskOpToInfinitive op) ++ " task"

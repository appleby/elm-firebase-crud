port module Main exposing (..)

import Bootstrap.Buttons exposing (..)
import Bootstrap.Forms exposing (..)
import Bootstrap.Grid exposing (..)
import Bootstrap.Navbar exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Navigation exposing (Location)
import String
import UrlParser exposing ((</>))


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port fetchTasks : () -> Cmd msg


port addTaskPort : Json.Encode.Value -> Cmd msg


port deleteTask : TaskId -> Cmd msg


port saveTaskPort : Json.Encode.Value -> Cmd msg


port signOutOk : (Bool -> msg) -> Sub msg


port signInOk : (User -> msg) -> Sub msg


port signInErr : (AuthError -> msg) -> Sub msg


port fetchTasksOk : (Json.Decode.Value -> msg) -> Sub msg


port addTaskOk : (Bool -> msg) -> Sub msg


port deleteTaskOk : (Bool -> msg) -> Sub msg


port saveTaskOk : (Bool -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , update = updateWithSignInCheck
        , view = view
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initRoute =
            parseLocation location
    in
        ( initModel initRoute, Cmd.none )


initModel : Route -> Model
initModel route =
    { tasks = []
    , route = route
    , apiPending = False
    , displayResult = Nothing
    , pendingTask = emptyTask
    , user = Nothing
    }


emptyTask : Task
emptyTask =
    { id = "", title = "", tags = [], freq = Daily }


type Frequency
    = Daily
    | Weekly
    | Monthly


type alias TaskId =
    String


type alias Task =
    { id : TaskId
    , title : String
    , tags : List String
    , freq : Frequency
    }


type alias DisplayResult =
    Result String String


type alias Model =
    { tasks : List Task
    , route : Route
    , apiPending : Bool
    , displayResult : Maybe DisplayResult
    , pendingTask : Task
    , user : Maybe User
    }


type Route
    = HomeRoute
    | TasksRoute
    | TaskEditRoute TaskId
    | TaskAddRoute
    | NotFoundRoute


routeToString : Route -> String
routeToString route =
    case route of
        HomeRoute ->
            "#"

        TasksRoute ->
            "#tasks"

        TaskAddRoute ->
            "#tasks/add"

        TaskEditRoute id ->
            "#tasks/" ++ id ++ "/edit"

        NotFoundRoute ->
            "#notfound"


findTaskById : TaskId -> List Task -> Maybe Task
findTaskById id =
    List.Extra.find (\t -> t.id == id)


resetPageState : Model -> Route -> Model
resetPageState model newRoute =
    let
        pending =
            case newRoute of
                TaskEditRoute taskId ->
                    findTaskById taskId model.tasks
                        |> Maybe.withDefault emptyTask

                _ ->
                    emptyTask
    in
        { model
            | pendingTask = pending
            , apiPending = False
            , displayResult = Nothing
        }


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map TaskAddRoute (UrlParser.s "tasks" </> UrlParser.s "add")
        , UrlParser.map TaskEditRoute (UrlParser.s "tasks" </> UrlParser.string </> UrlParser.s "edit")
        , UrlParser.map TasksRoute (UrlParser.s "tasks")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


freqOfString : String -> Result String Frequency
freqOfString str =
    case str of
        "daily" ->
            Ok Daily

        "weekly" ->
            Ok Weekly

        "monthly" ->
            Ok Monthly

        _ ->
            Err (str ++ " is not a valid Frequency")


freqToString : Frequency -> String
freqToString freq =
    case freq of
        Daily ->
            "daily"

        Weekly ->
            "weekly"

        Monthly ->
            "monthly"


addTask : Task -> Cmd Msg
addTask task =
    addTaskPort (encodeTask task)


saveTask : Task -> Cmd Msg
saveTask task =
    saveTaskPort (encodeTaskWithId task)


goto : Route -> Cmd Msg
goto route =
    Navigation.newUrl (routeToString route)


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


type Msg
    = OnLocationChange Location
    | Goto Route
    | FetchTasksDone (Result String (List Task))
    | EditTaskTitle String
    | EditTaskTags String
    | EditTaskFrequency String
    | SaveTask
    | SaveTaskDone Bool
    | AddTask
    | AddTaskDone Bool
    | DeleteTask Task
    | DeleteTaskDone Bool
    | SignIn
    | SignInDone (Result AuthError User)
    | SignOut
    | SignOutDone Bool


authRequired : Msg -> Bool
authRequired msg =
    case msg of
        SignIn ->
            False

        SignInDone _ ->
            False

        SignOut ->
            False

        SignOutDone _ ->
            False

        Goto HomeRoute ->
            False

        OnLocationChange location ->
            case parseLocation location of
                HomeRoute ->
                    False

                _ ->
                    True

        _ ->
            True


signedIn : Maybe User -> Bool
signedIn maybeUser =
    -- TODO: Maybe.Extra.isJust from elm-community?
    case maybeUser of
        Just _ ->
            True

        Nothing ->
            False


signedOut : Maybe User -> Bool
signedOut =
    -- TODO: Maybe.Extra.isNothing from elm-community?
    not << signedIn


updateWithSignInCheck : Msg -> Model -> ( Model, Cmd Msg )
updateWithSignInCheck msg model =
    if signedOut model.user && authRequired msg then
        ( model, signIn () )
    else
        update msg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn ->
            ( model, signIn () )

        SignInDone (Ok user) ->
            ({ model | user = Just user }
                ! [ fetchTasks (), goto TasksRoute ]
            )

        SignInDone (Err authErr) ->
            -- TODO: display error
            ( { model
                | user = Nothing
                , tasks = []
              }
            , goto HomeRoute
            )

        SignOut ->
            ( model, signOut () )

        SignOutDone True ->
            ( { model
                | user = Nothing
                , tasks = []
              }
            , goto HomeRoute
            )

        SignOutDone False ->
            -- TODO: display error
            ( model, Cmd.none )

        Goto newRoute ->
            ( model, goto newRoute )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                resetModel =
                    resetPageState model newRoute
            in
                ( { resetModel | route = newRoute }, Cmd.none )

        FetchTasksDone (Ok tasks) ->
            ( { model | tasks = tasks }, Cmd.none )

        FetchTasksDone (Err error) ->
            let
                _ =
                    Debug.log "failed to fetch tasks" error
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

        DeleteTask task ->
            ( updateModelForApiRequest model
            , deleteTask task.id
            )

        DeleteTaskDone succeeded ->
            handleTaskResult model Delete succeeded


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


type alias UserId =
    String


type alias UserInfo =
    { displayName : String
    , email : String
    , photoURL : String
    , providerId : String
    , uid : UserId
    }


type alias User =
    { displayName : String
    , email : String
    , photoURL : String
    , providerId : String
    , uid : UserId
    , providerData : List UserInfo
    , emailVerified : Bool
    , isAnonymous : Bool
    , refreshToken : String
    }


type alias AuthError =
    { code : Maybe String
    , message : Maybe String
    }


decodeTaskListFromValue : Json.Decode.Value -> Result String (List Task)
decodeTaskListFromValue =
    Json.Decode.decodeValue taskListDecoder


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ signInOk (SignInDone << Ok)
        , signInErr (SignInDone << Err)
        , signOutOk SignOutDone
        , fetchTasksOk (FetchTasksDone << decodeTaskListFromValue)
        , addTaskOk AddTaskDone
        , deleteTaskOk DeleteTaskDone
        , saveTaskOk SaveTaskDone
        ]


navLink : Route -> Route -> String -> Html Msg
navLink currentRoute linkTo linkText =
    let
        attrs =
            if currentRoute == linkTo then
                [ class "active" ]
            else
                []
    in
        li attrs [ a [ href (routeToString linkTo) ] [ text linkText ] ]


signInOut : Maybe User -> Html Msg
signInOut maybeUser =
    case maybeUser of
        Just user ->
            navbarList
                NavbarNav
                NavbarRight
                []
                [ li [] [ p [ class "navbar-text" ] [ text user.displayName ] ]
                , li [] [ a [ onClick SignOut ] [ text "Sign Out" ] ]
                ]

        Nothing ->
            navbarList
                NavbarNav
                NavbarRight
                []
                [ li [] [ a [ onClick SignIn ] [ text "Sign In" ] ] ]


navLinks : Route -> Maybe User -> List (Html Msg)
navLinks currentRoute maybeUser =
    if signedIn maybeUser then
        [ navLink currentRoute TasksRoute "List Tasks"
        , navLink currentRoute TaskAddRoute "Add Task"
        ]
    else
        []


myNavbar : Route -> Maybe User -> Html Msg
myNavbar currentRoute maybeUser =
    navbar
        DefaultNavbar
        [ class "navbar-static-top" ]
        [ container
            [ navbarHeader
                []
                [ navbarHamburger "#navbar"
                , navbarBrand
                    [ onClick (Goto HomeRoute) ]
                    [ text "timeslots" ]
                ]
            , navbarCollapse
                [ id "navbar" ]
                [ navbarList
                    NavbarNav
                    NavbarDefault
                    []
                    (navLinks currentRoute maybeUser)
                , signInOut maybeUser
                ]
            ]
        ]


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


viewTasks : List Task -> Html Msg
viewTasks tasks =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Title" ]
                , th [] [ text "Frequency" ]
                , th [] [ text "Tags" ]
                , th [] [ text "Action" ]
                ]
            ]
        , tbody [] (List.map viewTask tasks)
        ]


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


editTaskForm : Msg -> Task -> Bool -> Html Msg
editTaskForm submitMsg task apiPending =
    Bootstrap.Forms.form
        FormDefault
        [ onSubmit submitMsg ]
        [ formGroup FormGroupDefault
            [ formLabel [ for "taskTitle" ] [ text "Title" ]
            , formInput
                [ id "taskTitle"
                , value task.title
                , maxlength maxInputLength
                , onInput EditTaskTitle
                ]
                []
            ]
        , formGroup FormGroupDefault
            [ formLabel [ for "taskTags" ] [ text "Tags" ]
            , formInput
                [ id "taskTags"
                , value (String.join ", " task.tags)
                , maxlength maxInputLength
                , onInput EditTaskTags
                ]
                []
            ]
        , formGroup FormGroupDefault
            [ formLabel [ for "taskFrequency" ] [ text "Frequency" ]
            , frequencySelect task.freq EditTaskFrequency
            ]
        , if apiPending then
            btn BtnDefault
                []
                []
                [ type_ "submit", disabled True ]
                [ text "Save Task ", i [ class "fa fa-spinner fa-spin" ] [] ]
          else
            btn BtnDefault [] [] [ type_ "submit" ] [ text "Save Task" ]
        ]


emptyDiv : Html Msg
emptyDiv =
    div [] []


containerWithAlerts : Maybe DisplayResult -> Html Msg -> Html Msg
containerWithAlerts displayResult contents =
    container
        [ row [ showAlert displayResult ]
        , row [ contents ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        HomeRoute ->
            emptyDiv

        TasksRoute ->
            viewTasks model.tasks

        TaskAddRoute ->
            editTaskForm AddTask model.pendingTask model.apiPending

        TaskEditRoute taskId ->
            case findTaskById taskId model.tasks of
                Just _ ->
                    editTaskForm SaveTask model.pendingTask model.apiPending

                Nothing ->
                    -- TODO: return real error here
                    text ("Error: No task with id: " ++ taskId)

        NotFoundRoute ->
            -- TODO: Display 404
            emptyDiv


view : Model -> Html Msg
view model =
    div []
        [ myNavbar model.route model.user
        , containerWithAlerts model.displayResult (page model)
        ]

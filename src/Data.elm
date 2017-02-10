module Data exposing (..)

import List.Extra
import Navigation exposing (Location)
import UrlParser exposing ((</>), s, string, top)


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


goto : Route -> Cmd msg
goto route =
    Navigation.newUrl (routeToString route)


emptyTask : Task
emptyTask =
    { id = "", title = "", tags = [], freq = Daily }


findTaskById : TaskId -> List Task -> Maybe Task
findTaskById id =
    List.Extra.find (\t -> t.id == id)


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute top
        , UrlParser.map TaskAddRoute (s "tasks" </> s "add")
        , UrlParser.map TaskEditRoute (s "tasks" </> string </> s "edit")
        , UrlParser.map TasksRoute (s "tasks")
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

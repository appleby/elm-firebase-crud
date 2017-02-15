module Route exposing (Route(..), goto, parseLocation, toString)

import Data exposing (..)
import Navigation exposing (Location)
import UrlParser exposing ((</>), s, string, top)


type Route
    = HomeRoute
    | TasksRoute
    | TaskEditRoute TaskId
    | TaskAddRoute
    | NotFoundRoute


goto : Route -> Cmd msg
goto route =
    Navigation.newUrl (toString route)


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


toString : Route -> String
toString route =
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

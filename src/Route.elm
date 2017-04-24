module Route exposing (Route(..), goto, linkTo, parseLocation, toHref, toString)

import Data exposing (TaskId)
import Html exposing (Html, a)
import Html.Attributes
import Navigation
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


linkTo : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
linkTo route attributes content =
    let
        attrs =
            toHref route :: attributes
    in
        a attrs content


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute top
        , UrlParser.map TaskAddRoute (s "tasks" </> s "add")
        , UrlParser.map TaskEditRoute (s "tasks" </> string </> s "edit")
        , UrlParser.map TasksRoute (s "tasks")
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


toHref : Route -> Html.Attribute msg
toHref =
    Html.Attributes.href << toString


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

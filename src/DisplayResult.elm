module DisplayResult
    exposing
        ( DisplayResult
        , ResultDisplayer
        , containerWithAlerts
        , succ
        , fail
        , nothing
        )

import Bootstrap.Grid exposing (container, row)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class)


type alias DisplayResult =
    Result String String


type alias ResultDisplayer a =
    { a | displayResult : Maybe DisplayResult }


setResult : Maybe DisplayResult -> ResultDisplayer a -> ResultDisplayer a
setResult result model =
    { model | displayResult = result }


succ : String -> ResultDisplayer a -> ResultDisplayer a
succ msg =
    setResult (Just (Ok msg))


fail : String -> ResultDisplayer a -> ResultDisplayer a
fail msg =
    setResult (Just (Err msg))


nothing : ResultDisplayer a -> ResultDisplayer a
nothing =
    setResult Nothing


showAlert : Maybe DisplayResult -> Html msg
showAlert displayResult =
    case displayResult of
        Just (Ok msg) ->
            div [ class "alert alert-success" ]
                [ strong [] [ text "Ok! " ]
                , text msg
                ]

        Just (Err msg) ->
            div [ class "alert alert-danger" ]
                [ strong [] [ text "Error! " ]
                , text msg
                ]

        Nothing ->
            div [] []


containerWithAlerts : ResultDisplayer a -> Html msg -> Html msg
containerWithAlerts { displayResult } contents =
    container
        [ row [ showAlert displayResult ]
        , row [ contents ]
        ]

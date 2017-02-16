module DisplayResult exposing (DisplayResult, containerWithAlerts)

import Bootstrap.Grid exposing (container, row)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class)


type alias DisplayResult =
    Result String String


showAlert : Maybe DisplayResult -> Html msg
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


containerWithAlerts : Maybe DisplayResult -> Html msg -> Html msg
containerWithAlerts displayResult contents =
    container
        [ row [ showAlert displayResult ]
        , row [ contents ]
        ]

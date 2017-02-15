module Auth exposing (..)

import Bootstrap.Navbar exposing (..)
import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Ports exposing (..)
import Route exposing (Route(..))


type alias Model =
    { user : Maybe User }


initModel : Model
initModel =
    { user = Nothing }


type Msg
    = SignIn
    | SignOut
    | AuthStateChanged (Maybe User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn ->
            ( model, signIn () )

        SignOut ->
            ( model, signOut () )

        AuthStateChanged (Just user) ->
            ({ model | user = Just user } ! [ fetchTasks (), Route.goto TasksRoute ])

        AuthStateChanged Nothing ->
            ( { model | user = Nothing }, Route.goto HomeRoute )


authRequired : Msg -> Bool
authRequired _ =
    False


signedIn : Model -> Bool
signedIn model =
    -- TODO: Maybe.Extra.isJust from elm-community?
    case model.user of
        Just _ ->
            True

        Nothing ->
            False


signedOut : Model -> Bool
signedOut =
    -- TODO: Maybe.Extra.isNothing from elm-community?
    not << signedIn


signInOut : Model -> Html Msg
signInOut model =
    case model.user of
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

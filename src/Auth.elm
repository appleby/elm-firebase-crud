module Auth
    exposing
        ( Model
        , Msg
        , subscriptions
        , update
        , authRequired
        , initModel
        , signedIn
        , signedOut
        , signInOut
        )

import Bootstrap.Navbar
    exposing
        ( NavbarOptions(..)
        , NavbarListAdjustment(..)
        , navbarList
        )
import Data exposing (User, userName)
import Html exposing (Html, a, li, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ports exposing (authStateChanged, signIn, signOut)
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


authRequired : Msg -> Bool
authRequired _ =
    False


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ authStateChanged AuthStateChanged ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn ->
            ( model, signIn () )

        SignOut ->
            ( model, signOut () )

        AuthStateChanged (Just user) ->
            ( { model | user = Just user }, Route.goto TasksRoute )

        AuthStateChanged Nothing ->
            ( { model | user = Nothing }, Route.goto HomeRoute )


signedIn : Model -> Bool
signedIn model =
    -- Maybe.Extra.isJust from elm-community?
    case model.user of
        Just _ ->
            True

        Nothing ->
            False


signedOut : Model -> Bool
signedOut =
    -- Maybe.Extra.isNothing from elm-community?
    not << signedIn


signInOut : Model -> Html Msg
signInOut model =
    case model.user of
        Just user ->
            navbarList
                NavbarNav
                NavbarRight
                []
                [ li [] [ p [ class "navbar-text" ] [ text (userName user) ] ]
                , li [] [ a [ onClick SignOut ] [ text "Sign Out" ] ]
                ]

        Nothing ->
            navbarList
                NavbarNav
                NavbarRight
                []
                [ li [] [ a [ onClick SignIn ] [ text "Sign In" ] ] ]

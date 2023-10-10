module Frontend exposing (..)

import Auth
import Auth.Common
import Auth.Flow
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Html.Events
import Lamdera
import Tuple.Extra
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = always Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url nav =
    { nav = nav
    , user = Nothing
    , authFlow = Auth.Common.Idle
    , authRedirectBaseUrl = { url | query = Nothing, fragment = Nothing }
    }
        |> Tuple.Extra.pairWith Cmd.none


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.nav (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        RequestAuth ->
            Auth.Flow.signInRequested Auth.googleAuthMethodId model Nothing
                |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        LogIn user ->
            { model | user = Just user }
                |> Tuple.Extra.pairWith Cmd.none

        LogOut ->
            { model | user = Nothing }
                |> Tuple.Extra.pairWith Cmd.none

        AuthToFrontend authMsg ->
            Auth.updateFromBackend authMsg model


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.button [Html.Events.onClick RequestAuth] [Html.text "Hello World!"]  ]
            ]
        ]
    }

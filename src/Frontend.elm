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
import Url exposing (Url)

type alias Model =
    FrontendModel

type alias Msg =
    FrontendMsg

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


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url nav =
    { nav = nav
    , user = Nothing
    , authFlow = Auth.Common.Idle
    , authRedirectBaseUrl = { url | path = "", query = Nothing, fragment = Nothing }
    }
        |> Auth.handleCallback url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    let
                        (newModel, authCmd) =
                            Auth.handleCallback url model
                    in
                    ( newModel
                    , Cmd.batch
                        [ Nav.pushUrl model.nav (Url.toString url)
                        , authCmd
                        ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model , Cmd.none)

        ClickLogIn ->
            Auth.Flow.signInRequested Auth.authOMethodId model Nothing
                |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)

        ClickLogOut ->
            ( { model | authFlow = Auth.Common.Idle, user = Nothing }
            , Lamdera.sendToBackend RequestLogOut
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend msg model =
    case msg of
        AuthUpdate maybeUser ->
            { model | user = maybeUser }
                |> Tuple.Extra.pairWith Cmd.none


        AuthToFrontend authMsg ->
            Auth.updateFromBackend authMsg model



view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text ("Signed in as : " ++ (model.user |> Maybe.map (.info >> .email) |> Maybe.withDefault "None" ))
                , Html.button [Html.Events.onClick ClickLogIn] [Html.text "LogIn"]
                , Html.button [Html.Events.onClick ClickLogOut] [Html.text "LogOut"]
                ]
            ]
        ]
    }

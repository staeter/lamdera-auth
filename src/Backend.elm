module Backend exposing (..)

import Auth
import Auth.Flow
import Dict
import Lamdera exposing (ClientId, SessionId)
import Time
import Tuple.Extra
import Types exposing (..)
import Env
import Task

type alias Model =
    BackendModel

type alias Msg =
    BackendMsg


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect (ClientConnect PerformTimeTask)
        , Lamdera.onDisconnect ClientDisconnect
        , Time.every (60*1000) EveryMinute
        ]



init : ( Model, Cmd Msg )
init =
    ( { users = Dict.empty
      , sessions = Dict.empty
      , pendingAuths = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EveryMinute now ->
            (logoutExpiredAuths now model, Cmd.none)

        ClientConnect time sessionId clientId ->
            case time of
                PerformTimeTask ->
                    Time.now
                    |> Task.perform (\t -> ClientConnect (ReceivedTime t) sessionId clientId)
                    |> Tuple.pair model

                ReceivedTime now ->
                    clientConnect sessionId clientId now model


        ClientDisconnect sessionId clientId ->
            Dict.update
                sessionId
                ( Dict.remove clientId
                    |> Tuple.mapSecond
                    |> Maybe.map
                )
                model.sessions
                |> (\sessions -> { model | sessions = sessions })
                |> Tuple.Extra.pairWith Cmd.none

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg

clientConnect : Lamdera.SessionId -> Lamdera.ClientId -> Time.Posix -> Model -> (Model, Cmd Msg)
clientConnect sessionId clientId now model =
    let
        ( authStatus, clientDict ) =
            case Dict.get sessionId model.sessions of
                Nothing ->
                    ( LoggedOut, Dict.singleton clientId now )

                Just ( authSt, cliDict ) ->
                    ( authSt, Dict.insert clientId now cliDict )

        cmd =
            case authStatus of
                LoggedOut ->
                    Cmd.none

                LoggedIn _ _ _ userId ->
                    Dict.get userId model.users
                        |> Maybe.map (\user -> Lamdera.sendToFrontend sessionId (AuthUpdate <| Just user))
                        |> Maybe.withDefault Cmd.none
    in
    { model | sessions = Dict.insert sessionId ( authStatus, clientDict ) model.sessions }
        |> Tuple.Extra.pairWith cmd

logoutExpiredAuths : Time.Posix -> Model -> Model
logoutExpiredAuths now model =
    { model
    | sessions =
        Dict.map
            ((\_ (authStatus, d) ->
                case authStatus of
                    LoggedOut ->
                        (LoggedOut, d)

                    LoggedIn _ loginTime _ _ ->
                        if Time.posixToMillis now >= Time.posixToMillis loginTime + (Env.authOIdTokenExpiration * 1000)
                        then
                            (LoggedOut, d)
                        else
                            (authStatus, d)
            )
            )
            model.sessions
    }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        AuthToBackend authMsg ->
            Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authMsg model

        RequestLogOut ->
            { model | sessions = Dict.update sessionId (always LoggedOut |> Tuple.mapFirst |> Maybe.map) model.sessions }
                |> Tuple.Extra.pairWith Cmd.none

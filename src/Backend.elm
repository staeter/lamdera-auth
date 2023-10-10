module Backend exposing (..)

import Auth
import Auth.Flow
import Dict
import Lamdera exposing (ClientId, SessionId)
import Time
import Tuple.Extra
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnect
        , Lamdera.onDisconnect ClientDisconnect
        , Time.every 1 Tick
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( { now = Time.millisToPosix 0
      , users = Dict.empty
      , sessions = Dict.empty
      , pendingAuths = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Tick now ->
            ( { model | now = now }, Cmd.none )

        ClientConnect sessionId clientId ->
            let
                ( authStatus, clientDict ) =
                    case Dict.get sessionId model.sessions of
                        Nothing ->
                            ( LoggedOut, Dict.singleton clientId model.now )

                        Just ( authSt, cliDict ) ->
                            ( authSt, Dict.insert clientId model.now cliDict )

                cmd =
                    case authStatus of
                        LoggedOut ->
                            Cmd.none

                        LoggedIn _ _ _ userId ->
                            Dict.get userId model.users
                                |> Maybe.map (\user -> Lamdera.sendToFrontend clientId (LogIn user))
                                |> Maybe.withDefault Cmd.none
            in
            { model | sessions = Dict.insert sessionId ( authStatus, clientDict ) model.sessions }
                |> Tuple.Extra.pairWith cmd

        ClientDisconnect sessionId clientId ->
            Dict.update
                sessionId
                ((\val ->
                    case val of
                        ( LoggedOut, clientDict ) ->
                            ( LoggedOut, Dict.remove clientId clientDict )

                        ( loggedIn, clientDict ) ->
                            ( loggedIn, Dict.remove clientId clientDict )
                 )
                    |> Maybe.map
                )
                model.sessions
                |> (\sessions -> { model | sessions = sessions })
                |> Tuple.Extra.pairWith Cmd.none

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        AuthToBackend authMsg ->
            Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authMsg model

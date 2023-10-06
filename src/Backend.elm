module Backend exposing (..)

import Auth.Flow
import Dict
import Lamdera exposing (ClientId, SessionId)
import Time
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnect
        , Lamdera.onDisconnect ClientDisconnect
        , Time.every 1 Tick
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!" }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        ClientConnect sessionId clientId ->
            Dict.update
                sessionId
                (\val ->
                    case val of
                        Nothing ->
                            ( Nothing, Dict.singleton clientId model.time ) |> Just

                        Just ( user, clientDict ) ->
                            ( user, Dict.insert clientId model.time clientDict ) |> Just
                )
                model.clients
                |> (\clients -> { model | clients = clients })
                |> (\model -> ( model, Cmd.none ))

        ClientDisconnect sessionId clientId ->
            Dict.update
                sessionId
                (Maybe.map
                    (\val ->
                        case val of
                            ( Nothing, clientDict ) ->
                                let
                                    newClientDict =
                                        Dict.remove clientId clientDict
                                in
                                if Dict.isEmpty newClientDict then
                                    Nothing

                                else
                                    Just ( Nothing, newClientDict )

                            ( Just user, clientDict ) ->
                                ( user, Dict.remove clientId clientDict ) |> Just
                    )
                )
                model.clients
                |> (\clients -> { model | clients = clients })
                |> (\model -> ( model, Cmd.none ))

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate Auth.backendUpdateConfig authMsg


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        AuthToBackend authMsg ->
            Auth.Flow.updateFromFrontend Auth.backendUpdateConfig clientId sessionId authMsg model

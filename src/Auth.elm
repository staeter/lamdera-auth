module Auth exposing (..)

import Auth.Common exposing (MethodId)
import Auth.Flow
import Auth.Method.OAuthGoogle
import Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)
import User


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods = [ googleAuthMethod ]
    , renewSession = refreshAuth
    }


backendConfig : BackendModel -> Auth.Flow.BackendUpdateConfig FrontendMsg BackendMsg ToFrontend FrontendModel BackendModel
backendConfig model =
    { asToFrontend = AuthToFrontend
    , asBackendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = model
    , loadMethod =
        \methodId ->
            if methodId == googleAuthMethodId then
                Just googleAuthMethod

            else
                Nothing
    , handleAuthSuccess = handleAuthSuccess model
    , renewSession = refreshAuth
    , logout = refreshAuth
    , isDev = Env.mode == Env.Development
    }


googleAuthMethod : Auth.Common.Method FrontendMsg BackendMsg FrontendModel BackendModel
googleAuthMethod =
    Auth.Method.OAuthGoogle.configuration
        Env.googleAppClientId
        Env.googleAppClientSecret

googleAuthMethodId : Auth.Common.MethodId
googleAuthMethodId = "OAuthGoogle"

handleAuthSuccess :
    BackendModel
    -> Auth.Common.SessionId
    -> Auth.Common.ClientId
    -> Auth.Common.UserInfo
    -> MethodId
    -> Maybe Auth.Common.Token
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess model sessionId clientId userInfo methodId token now =
    let
        userId =
            User.infoToId userInfo

        clientDict =
            Dict.get sessionId model.sessions
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Dict.empty

        newAuthStatus =
            LoggedIn methodId now token userId

        newClientDict =
            Dict.insert clientId now clientDict
    in
    ( { model
        | sessions =
            Dict.insert
                sessionId
                ( newAuthStatus, newClientDict )
                model.sessions
        , users =
            if Dict.member userId model.users then
                model.users

            else
                Dict.insert userId (User.init userInfo) model.users
      }
    , sendAuthToWholeSession sessionId model
    )


sendAuthToWholeSession : SessionId -> BackendModel -> Cmd BackendMsg
sendAuthToWholeSession sessionId model =
    case Dict.get sessionId model.sessions of
        Nothing ->
            Cmd.none

        Just ( LoggedIn _ _ _ userId, _ ) ->
            case Dict.get userId model.users of
                Nothing ->
                    Cmd.none

                Just user ->
                    Lamdera.sendToFrontend sessionId (LogIn user)

        Just ( LoggedOut, _ ) ->
            Lamdera.sendToFrontend sessionId LogOut


refreshAuth : SessionId -> ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
refreshAuth sessionId _ model =
    sendAuthToWholeSession sessionId model
        |> Tuple.pair model


updateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err

        Auth.Common.AuthSessionChallenge reason ->
            Debug.todo "Auth.Common.AuthSessionChallenge"

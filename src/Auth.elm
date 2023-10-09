module Auth exposing (..)

import Auth.Common
import Auth.Method.EmailMagicLink
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
import Types exposing (..)
import Auth.Flow
import Lamdera
import Env
import Dict
import User

config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods = [ GoogleAuthMethod ]
    }


backendConfig :
    BackendModel
    -> (Auth.Common.SessionId
        -> Auth.Common.ClientId
        -> Auth.Common.UserInfo
        -> MethodId
        -> Maybe Auth.Common.Token
        -> Time.Posix
        -> BackendModel
        -> (BackendModel, Cmd BackendMsg)
    )
    ->
    -> Auth.Flow.BackendUpdateConfig FrontendMsg BackendMsg ToFrontend FrontendModel BackendModel
backendConfig model handleAuthSuccess =
    { asToFrontend = AuthToFrontend
    , asBackendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = model
    , loadMethod =
        \ methodId ->
            if methodId == "OAuthGoogle"
            then Just GoogleAuthMethod
            else Nothing
    , handleAuthSuccess = (handleAuthSuccess model)
    , renewSession = renewSession
    , logout = logout
    , isDev = Env.mode == Env.Development
    }

handleAuthSuccess :
    BackendModel
    -> Auth.Common.SessionId
        -> Auth.Common.ClientId
        -> Auth.Common.UserInfo
        -> MethodId
        -> Maybe Auth.Common.Token
        -> Time.Posix
        -> (BackendModel, Cmd BackendMsg)
handleAuthSuccess sessionId clientId userInfo methodId token now model =
    let
        userId = User.infoToId userInfo
        clientDict =
            Dict.get sessionId model.sessions
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Dict.empty

        newAuthStatus =
            LoggedIn methodId now token userId

        newClientDict =
            Dict.insert clientId now clientDict
    in
    ({model
    | sessions =
        Dict.insert
            sessionId
            (newAuthStatus, newClientDict)
            model.sessions
    , users =
        if Dict.member userId model.users
        then model.users
        else Dict.insert userId (User.init userInfo) model.users
    }
    , Dict.toList newClientDict
        |> List.map (\(clientId,_) -> AuthSuccess >> Lamdera.sendToFrontend clientId)
        |> Cmd.batch
    )

renewSession : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession sessionId clientId model =
    case Dict.get sessionId model.sessions of
        Just (authStatus, clientDict) ->
            -- A session already existed, renew it
            case model.users |> Dict.get session.username of
                Just user ->
                    ( model
                    , Data.User.sendAuthSuccessWithAccount (Effect.Lamdera.sessionIdFromString sessionId) user session model |> toCmd
                    )

                Nothing ->
                    -- The session is for a user that doesn't exist, remove and force re-auth
                    ( { model | sessions = sessionId |> removeSession model }
                    , forceUserToLoginAgain sessionId Auth.Common.AuthSessionExpired
                    )

        Nothing ->
            -- No session exists, user is not logged in.
            ( model, forceUserToLoginAgain sessionId Auth.Common.AuthSessionMissing )

GoogleAuthMethod =
    Auth.Method.OAuthGoogle.configuration Env.googleAppClientId Env.googleAppClientSecret

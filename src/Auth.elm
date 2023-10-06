module Auth exposing (..)

import Auth.Common
import Auth.Method.EmailMagicLink
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
import Types exposing (..)
import Auth.Flow


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods =
        [ Auth.Method.EmailMagicLink.configuration
        , Auth.Method.OAuthGithub.configuration Config.githubAppClientId Config.githubAppClientSecret
        , Auth.Method.OAuthGoogle.configuration Config.googleAppClientId Config.googleAppClientSecret
        ]
    }


backendUpdateConfig : BackendModel -> Auth.Flow.BackendUpdateConfig FrontendMsg BackendMsg ToFrontend FrontendModel BackendModel
backendUpdateConfig backendModel =
    { asToFrontend = AuthToFrontend
    , asBackendMsg = AuthBackendMsg
    , sendToFrontend : Auth.Common.SessionId -> toFrontend -> Cmd backendMsg
    , backendModel = backendModel
    , loadMethod : Auth.Common.MethodId -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel)
    , handleAuthSuccess :
        Auth.Common.SessionId
        -> Auth.Common.ClientId
        -> Auth.Common.UserInfo
        -> MethodId
        -> Maybe Auth.Common.Token
        -> Time.Posix
        -> ( { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }, Cmd backendMsg )
    , renewSession : Auth.Common.SessionId -> Auth.Common.ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    , logout : Auth.Common.SessionId -> Auth.Common.ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    , isDev = True --! Dev mode
    }

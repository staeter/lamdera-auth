module Types exposing (..)

import Auth.Common
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Time
import Url exposing (Url)
import User exposing (User, UserId)


-- Models

type alias FrontendModel =
    { nav : Nav.Key
    , user : Maybe User
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    }


type alias BackendModel =
    { users : Dict UserId User
    , sessions : Dict SessionId ( AuthStatus, Dict ClientId Time.Posix )
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    }


-- Msgs

type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | ClickLogIn
    | ClickLogOut


type BackendMsg
    = EveryMinute Time.Posix
    | ClientConnect Time Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnect Lamdera.SessionId Lamdera.ClientId
    | AuthBackendMsg Auth.Common.BackendMsg


-- Pipe

type ToBackend
    = AuthToBackend Auth.Common.ToBackend
    | RequestLogOut

type ToFrontend
    = AuthUpdate (Maybe User)
    | AuthToFrontend Auth.Common.ToFrontend


-- Util

type AuthStatus
    = LoggedOut
    | LoggedIn Auth.Common.MethodId Time.Posix (Maybe Auth.Common.Token) UserId

type Time
    = PerformTimeTask
    | ReceivedTime Time.Posix

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


type alias FrontendModel =
    { nav : Nav.Key
    , user : Maybe User
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    }


type alias BackendModel =
    { now : Time.Posix
    , users : Dict UserId User
    , sessions : Dict SessionId ( AuthStatus, Dict ClientId Time.Posix )
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    }


type AuthStatus
    = LoggedOut
    | LoggedIn Auth.Common.MethodId Time.Posix (Maybe Auth.Common.Token) UserId


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | RequestAuth


type BackendMsg
    = Tick Time.Posix
    | ClientConnect Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnect Lamdera.SessionId Lamdera.ClientId
    | AuthBackendMsg Auth.Common.BackendMsg

type ToBackend
    = AuthToBackend Auth.Common.ToBackend

type ToFrontend
    = LogIn User
    | LogOut
    | AuthToFrontend Auth.Common.ToFrontend

module Types exposing (..)

import Auth.Common
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera
import Set exposing (Set)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Nav.Key
    , message : String
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    }


type Auth
    = Basic
    | Admin


type alias User =
    { name : String
    }


type alias UserId =
    String


type alias BackendModel =
    { time : Time.Posix
    , users : Dict UserId User
    , clients : Dict SessionId ( Maybe UserId, Dict ClientId Time.Posix )
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | AuthFrontendMsg Auth.Common.FrontendMsg


type ToBackend
    = NoOpToBackend
    | AuthToBackend Auth.Common.ToBackend


type BackendMsg
    = NoOpBackendMsg
    | Tick Time.Posix
    | ClientConnect Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnect Lamdera.SessionId Lamdera.ClientId
    | AuthBackendMsg Auth.Common.BackendMsg


type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend

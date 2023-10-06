module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Auth.Common
import Dict exposing (Dict)
import Lamdera

type alias FrontendModel =
    { key : Nav.Key
    , message : String
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    }

type alias User =
    { auth : Auth
    }

type Auth
    = Basic
    | Admin

type alias BackendModel =
    { message : String
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
    | AuthBackendMsg Auth.Common.BackendMsg

type ToFrontend
    = NoOpToFrontend
    | AuthToFrontend Auth.Common.ToFrontend

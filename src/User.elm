module User exposing (..)

import Auth.Common


type alias UserId =
    String


type alias User =
    { info : Auth.Common.UserInfo
    , role : Role
    }


type Role
    = Basic
    | Admin


infoToId : Auth.Common.UserInfo -> UserId
infoToId userInfo =
    userInfo.email


init : Auth.Common.UserInfo -> User
init userInfo =
    { info = userInfo
    , role = Basic
    }

module User exposing (..)

import Auth.Common


type alias UserId =
    String


type alias User =
    { info : Auth.Common.UserInfo
    , auth : Auth
    }


type Auth
    = Basic
    | Admin


infoToId : Auth.Common.UserInfo -> UserId
infoToId userInfo =
    userInfo.email ++ "::" ++ userInfo.name ++ "::" ++ userInfo.username


init : Auth.Common.UserInfo -> User
init userInfo =
    { info = userInfo
    , auth = Basic
    }

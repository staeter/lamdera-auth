module Config exposing (..)

import Env


googleAppClientId =
    case Env.mode of
        Env.Production ->
            Env.googleAppClientId

        _ ->
            ""


googleAppClientSecret =
    case Env.mode of
        Env.Production ->
            Env.googleAppClientSecret

        _ ->
            ""

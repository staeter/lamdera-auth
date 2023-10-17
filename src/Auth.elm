module Auth exposing (..)

import Auth.Common exposing (MethodId)
import Auth.Flow
import Auth.Method.OAuthAuth0
import Dict
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)
import User
import Env
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query

config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods = [ authOMethod ]
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
            if methodId == authOMethodId then
                Just authOMethod

            else
                Nothing
    , handleAuthSuccess = handleAuthSuccess model
    , renewSession = refreshAuth
    , logout = refreshAuth
    , isDev = Env.mode == Env.Development
    }

authOMethod : Auth.Common.Method FrontendMsg BackendMsg FrontendModel BackendModel
authOMethod =
    Auth.Method.OAuthAuth0.configuration
        Env.authOClientId
        Env.authOClientSecret
        Env.authOAppTenant

authOMethodId : Auth.Common.MethodId
authOMethodId =
    "OAuthAuth0"


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

        newModel =
            { model
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
    in
    ( newModel
    , refreshFrontendAuth sessionId newModel
    )


refreshFrontendAuth : SessionId -> BackendModel -> Cmd BackendMsg
refreshFrontendAuth sessionId model =
    case Dict.get sessionId model.sessions of
        Nothing ->
            Cmd.none

        Just ( LoggedIn _ _ _ userId, _ ) ->
            case Dict.get userId model.users of
                Nothing ->
                    Cmd.none

                Just user ->
                    Lamdera.sendToFrontend sessionId (AuthUpdate <| Just user)

        Just ( LoggedOut, _ ) ->
            Lamdera.sendToFrontend sessionId (AuthUpdate Nothing)


refreshAuth : SessionId -> ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
refreshAuth sessionId _ model =
    refreshFrontendAuth sessionId model
        |> Tuple.pair model


updateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err

        Auth.Common.AuthSessionChallenge reason ->
            Debug.todo "Auth.Common.AuthSessionChallenge"

-- callback

type CallbackRoute
    = LogInCallback Auth.Common.MethodId {code : Maybe String, state : Maybe String}

handleCallback : Url -> FrontendModel -> (FrontendModel, Cmd FrontendMsg)
handleCallback url model =
    let
        callbackRouteParser =
            Url.Parser.oneOf
                [ Url.Parser.map
                    (\ method code state ->
                        LogInCallback method {code = code , state = state}
                    )
                    ( Url.Parser.s "login"
                    </> Url.Parser.string
                    </> Url.Parser.s "callback"
                    <?> Url.Parser.Query.string "code"
                    <?> Url.Parser.Query.string "state"
                    )
                ]
    in
    case Url.Parser.parse callbackRouteParser url of
        Nothing ->
            (model, Cmd.none)

        Just (LogInCallback method _) ->
            Auth.Flow.init
                model
                method
                url
                model.nav
                (AuthToBackend >> Lamdera.sendToBackend)

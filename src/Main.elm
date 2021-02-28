{-
the majority of the auth code in this file 
was pulled off the oath2 package demo
github with little alteration
-}

port module Main exposing (main)


import Base64.Encode as Base64
import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Delay exposing (TimeUnit(..), after)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import OAuth
import OAuth.Implicit as OAuth
import Url exposing (Protocol(..), Url)
import Json.Decode exposing (Decoder)
import Json.Decode exposing (map7)
import Bytes.Decode exposing (decode)
import Helpers exposing (..) 

type alias FlagObject = 
    Maybe (List Int)

main : Program FlagObject Model Msg
main =
    application
        { init =
            Maybe.map convertBytes >> init
        , update =
            update
        , subscriptions =
            always <| randomBytes GotRandomBytes
        , onUrlRequest =
            always NoOp
        , onUrlChange =
            always NoOp
        , view =
            view
                { title = "SpotifyConnectionVerifier"
                , btnClass = class "CTA__secondary--FVl7a"
                }
        }


configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = "api.spotify.com", path = "/v1/me" }
    , accountPlayerEndpoint = 
        { defaultHttpsUrl | host = "api.spotify.com", path = "/v1/me/player/devices" }
    , accountPlayerDecoder = 
        getDeviceList      
    , userInfoDecoder =
       Json.map UserInfo
            (Json.field "display_name" Json.string) 
    , clientId =
        "cfe2208fcad346cda755e6f65a7c171e"
    , scope =
        ["user-read-playback-state"]
    }

--decode device list
decodeDeviceList : Decoder AccountPlayer
decodeDeviceList = 
    Json.map7 
    AccountPlayer
        (Json.Decode.at ["id"] Json.string) 
        (Json.Decode.at ["is_active"] Json.bool)
        (Json.Decode.at ["is_private_session"] Json.bool)
        (Json.Decode.at ["is_restricted"] Json.bool)
        (Json.Decode.at ["name"] Json.string)
        (Json.Decode.at ["type"] Json.string)
        (Json.Decode.at ["volume_percent"] Json.int)


getDeviceList : Decoder (List AccountPlayer)
getDeviceList = 
    Json.Decode.at ["devices"]  (Json.Decode.list decodeDeviceList)

--
-- Model
--

type alias Model =
    { redirectUri : Url
    , flow : Flow
    }

type Flow
    = Idle
    | Authorized OAuth.Token
    | Done UserInfo
    | GotPlayers (List AccountPlayer)
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo
    | ErrHTTPGetAccountPlayerInfo


type alias UserInfo =
    { name : String}

type alias AccountPlayer = 
    {id: String
    ,is_active: Bool
    ,is_private_session: Bool 
    ,is_restricted: Bool 
    ,name: String 
    ,playerType: String 
    ,volume_percent: Int
    }



type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Json.Decoder UserInfo
    , accountPlayerEndpoint : Url 
    , accountPlayerDecoder : Json.Decoder (List AccountPlayer)
    , clientId : String
    , scope : List String
    }


{-| During the authentication flow, we'll run twice into the `init` function:
  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.
  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.
When query params are present (and valid), we consider the user `Authorized`.
-}
init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case OAuth.parseToken origin of
        OAuth.Empty ->
            ( { flow = Idle, redirectUri = redirectUri }
            , Cmd.none
            )

        -- It is important to set a `state` when making the authorization request
        -- and to verify it after the redirection. The state can be anything but its primary
        -- usage is to prevent cross-site request forgery; at minima, it should be a short,
        -- non-guessable string, generated on the fly.
        --
        -- We remember any previously generated state  state using the browser's local storage
        -- and give it back (if present) to the elm application upon start
        OAuth.Success { token, state } ->
            case mflags of
                Nothing ->
                    ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                    , clearUrl
                    )

                Just flags ->
                    if state /= Just flags.state then
                        ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                        , clearUrl
                        )

                    else
                        ( { flow = Authorized token, redirectUri = redirectUri }
                        , Cmd.batch
                            -- Artificial delay to make the live demo easier to follow.
                            -- In practice, the access token could be requested right here.
                            [ after 0 Millisecond AccountPlayerInfoRequested--UserInfoRequested
                            , clearUrl
                            ]
                        )

        OAuth.Error error ->
            ( { flow = Errored <| ErrAuthorization error, redirectUri = redirectUri }
            , clearUrl
            )



--
-- Msg
--


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthorizationSuccess)
    | UserInfoRequested
    | AccountPlayerInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | GotAccountPlayerInfo (Result Http.Error (List AccountPlayer))
    | SignOutRequested


getUserInfo : Configuration -> OAuth.Token -> Cmd Msg
getUserInfo { userInfoDecoder, userInfoEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

getAccountPlayers : Configuration -> OAuth.Token -> Cmd Msg
getAccountPlayers {accountPlayerDecoder, accountPlayerEndpoint} token = 
    Http.request 
        { method = "GET"
        , body = Http.emptyBody 
        , headers = OAuth.useToken token []
        , url = Url.toString accountPlayerEndpoint
        , expect = Http.expectJson GotAccountPlayerInfo accountPlayerDecoder 
        , timeout = Nothing
        , tracker = Nothing
        }



port genRandomBytes : Int -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg

port clientIdentifier : String -> Cmd msg

--
-- Update
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Authorized token, AccountPlayerInfoRequested) ->
            accountPlayerInfoRequested model token    

        ( Authorized _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Authorized _, GotAccountPlayerInfo accountPlayerResponse) ->
            gotAccountPlayerInfo model accountPlayerResponse

        ( Done _, SignOutRequested ) ->
            signOutRequested model

        _ ->
            noOp model


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Idle }
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd Msg )
userInfoRequested model token =
    ( { model | flow = Authorized token }
    , getUserInfo configuration token
    )

accountPlayerInfoRequested : Model -> OAuth.Token -> (Model, Cmd Msg)
accountPlayerInfoRequested model token = 
    ( {model | flow = Authorized token}
    , getAccountPlayers configuration token
    )


gotUserInfo : Model -> Result Http.Error UserInfo -> ( Model, Cmd Msg )
gotUserInfo model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            ( { model | flow = Errored ErrHTTPGetUserInfo }
            , Cmd.none
            )

        Ok userInfo ->
            ( { model | flow = Done userInfo }
            , Cmd.none
            )

gotAccountPlayerInfo : Model -> Result Http.Error (List AccountPlayer) -> (Model, Cmd Msg)
gotAccountPlayerInfo model accountPlayerResponse = 
    let
        _ = Debug.log "getPlayersResponse" accountPlayerResponse 
        
    in
    case accountPlayerResponse of 
        Err _ -> 
            (
                { model | flow = Errored ErrHTTPGetAccountPlayerInfo }
                , Cmd.none
            )
        Ok accountPlayer ->
            ({ model | flow = GotPlayers accountPlayer }
            , Cmd.none
            )


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )



--
-- View
--


type alias ViewConfiguration msg =
    { title : String
    , btnClass : Attribute msg
    }


view : ViewConfiguration Msg -> Model -> Document Msg
view ({ title } as config) model =
    { title = title
    , body = viewBody config model
    }


viewBody : ViewConfiguration Msg -> Model -> List (Html Msg)
viewBody config model =
    [ div [ class "flex", class "flex-column", class "flex-space-around"] <|
        case model.flow of
            Idle ->
                div [ class "flex" ]
                    [ viewAuthorizationStep False
                    , viewStepSeparator False
                    , viewGetUserInfoStep False
                    ]
                    :: viewIdle config

            Authorized _ ->
                div [ class "flex" ]
                    [ viewAuthorizationStep True
                    , viewStepSeparator True
                    , viewGetUserInfoStep False
                    ]
                    :: viewAuthorized

            Done userInfo ->
                div [ class "flex" ]
                    [ viewAuthorizationStep True
                    , viewStepSeparator True
                    , viewGetUserInfoStep True
                    ]
                    :: viewUserInfo config userInfo

            GotPlayers accountPlayerInfo ->
                div [ class "flex"]
                    [ viewAuthorizationStep True
                    , viewStepSeparator True 
                    , viewGetAccountPlayerInfoStep True 
                    ]
                    :: viewAccountPlayerInfo config accountPlayerInfo

            Errored err ->
                div [ class "flex" ]
                    [ viewErroredStep
                    ]
                    :: viewErrored err
    ]


viewIdle : ViewConfiguration Msg -> List (Html Msg)
viewIdle { btnClass } =
    [ button 
        [ onClick SignInRequested, btnClass ]
        [ text "Sign in" ]
    ]


viewAuthorized : List (Html Msg)
viewAuthorized =
    [ span [] [ text "Getting user info..." ]
    ]


viewUserInfo : ViewConfiguration Msg -> UserInfo -> List (Html Msg)
viewUserInfo { btnClass } { name } =
    [ div [ class "flex", class "flex-column" ]
        [ p [] [ text name ]
        , div []
            [ button
                [ onClick SignOutRequested, btnClass ]
                [ text "Sign out" ]
            ]
        ]
    ]

viewAccountPlayerInfo : ViewConfiguration Msg -> (List AccountPlayer) -> List (Html Msg)
viewAccountPlayerInfo {btnClass } ls = 
    [ div [class "flex", class "flex-column"]
        [ul []
        (List.map (\l -> li [] [(text l.id),(text ", "),(text l.name),(text ", "),(text (boolToString l.is_active)) ]) ls)
        , div []
        [ button
                [ onClick SignOutRequested, btnClass ]
                [ text "Sign out" ]
            ]
        ]
    ]


viewErrored : Error -> List (Html Msg)
viewErrored error =
    [ span [ class "span-error" ] [ viewError error ] ]


viewError : Error -> Html Msg
viewError e =
    text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."

            ErrHTTPGetAccountPlayerInfo ->
                "Unable to retrive account player info: HTTP request failed."


viewAuthorizationStep : Bool -> Html Msg
viewAuthorizationStep isActive =
    viewStep isActive ( "Authorization", style "left" "-110%" )


viewGetUserInfoStep : Bool -> Html Msg
viewGetUserInfoStep isActive =
    viewStep isActive ( "Get User Info", style "left" "-135%" )

viewGetAccountPlayerInfoStep : Bool -> Html Msg 
viewGetAccountPlayerInfoStep isActive = 
    viewStep isActive ("Get Account Play Info", style "left" "-135%")


viewErroredStep : Html Msg
viewErroredStep =
    div
        [ class "step", class "step-errored" ]
        [ span [ style "left" "-50%" ] [ text "Errored" ] ]


viewStep : Bool -> ( String, Attribute Msg ) -> Html Msg
viewStep isActive ( step, position ) =
    let
        stepClass =
            class "step"
                :: (if isActive then
                        [ class "step-active" ]

                    else
                        []
                   )
    in
    div stepClass [ span [ position ] [ text step ] ]


viewStepSeparator : Bool -> Html Msg
viewStepSeparator isActive =
    let
        stepClass =
            class "step-separator"
                :: (if isActive then
                        [ class "step-active" ]

                    else
                        []
                   )
    in
    span stepClass []
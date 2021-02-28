module Helpers exposing (..)


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

boolToString : Bool -> String
boolToString input = 
    if input == True 
        then "True"
        else "False"

toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
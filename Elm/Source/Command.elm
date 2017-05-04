module Command exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as P
import Json.Encode as Enc

type alias Flags =
    { webSocketUrl : String
    }

main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" flags.webSocketUrl "", Cmd.none )


type alias Model =
    { currentCommand : String
    , webSocketUrl : String
    , resp : String
    }

type Msg
    = ServerMsg String
    | SendCommand
    | KeyPressed Int
    | UpdateText String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerMsg json ->
            ( { model | resp = Debug.log "Resp" json }, Cmd.none )
        SendCommand ->
            ( model
            , WebSocket.send model.webSocketUrl <| "cmd/" ++ model.currentCommand
            )
        UpdateText text ->
            ( { model
                | currentCommand = text}
            , Cmd.none)
        KeyPressed code ->
            ( always model <| Debug.log "Key" code, Cmd.none)

view : Model -> Html Msg
view model =
    div [] 
    [ pre [] [ text model.resp ]
    , Html.form [ onSubmit SendCommand ]
        [ input [ onInput UpdateText ] [ text model.currentCommand ]
        , button [] [ text "Send!" ]
        ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.webSocketUrl ServerMsg

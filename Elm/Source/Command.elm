module Command exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline as P

type alias Flags =
    { webSocketUrl : String
    }

type alias Resp =
    { res : Maybe String
    , perms : Maybe (List String)
    }

decodeResp : Decoder Resp
decodeResp =
    P.decode Resp
        |> P.custom (Decode.maybe (Decode.field "res" Decode.string))
        |> P.custom (Decode.maybe (Decode.at ["you", "info", "perms"] <| Decode.list Decode.string))

main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" flags.webSocketUrl [] Nothing, Cmd.none )

type alias Model =
    { currentCommand : String
    , webSocketUrl : String
    , resp : List String
    , perms : Maybe (List String)
    }

type Msg
    = ServerMsg String
    | SendCommand
    | SendWSCommand
    | KeyPressed Int
    | UpdateText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerMsg json ->
            let result = decodeString decodeResp json
            in case result of
                Ok resp ->
                    ( { model
                    | resp =
                        case resp.res of
                            Just res -> List.append model.resp [res]
                            Nothing -> model.resp
                    , perms = resp.perms
                    }, Cmd.none )
                Err _ -> ( model, Cmd.none )

        SendCommand ->
            ( { model | currentCommand = "" }
            , WebSocket.send model.webSocketUrl <| "cmd/" ++ model.currentCommand
            )
        SendWSCommand ->
            ( { model | currentCommand = "" }
            , WebSocket.send model.webSocketUrl <| model.currentCommand
            )
        UpdateText text ->
            ( { model
                | currentCommand = text}
            , Cmd.none)
        KeyPressed code ->
            ( always model <| Debug.log "Key" code, Cmd.none)

view : Model -> Html Msg
view model =
    case model.perms of
        Nothing -> pre [] [text "Connecting..."]
        Just perms ->
            if List.member "command" perms then
                div []
                (
                    List.map (\resp -> div [] [pre [] [text resp], hr [] []]) model.resp ++
                    [ Html.form [ onSubmit SendCommand ]
                        [ input [ onInput UpdateText, style [("width", "100%")] ] [ text model.currentCommand ]
                        , button [] [ text "Send!" ]
                        ]
                    , button [ onClick SendWSCommand ] [ text "Send as WebSocket command" ]
                    ]
                )
            else
                pre [] [text "You do not have access to moderator commands."]

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.webSocketUrl ServerMsg


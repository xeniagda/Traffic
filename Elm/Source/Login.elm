import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import WebSocket

import Navigation exposing (load)

main = Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Flags = 
    { webSocketUrl : String
    }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( 
        { webSocketUrl = flags.webSocketUrl
        , name = ""}
    , Cmd.none)

type alias Model =
    { name : String
    , webSocketUrl : String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeName name -> ( { model | name = name }, Cmd.none )
        Submit -> ( model
                  , Cmd.batch 
                    [ WebSocket.send model.webSocketUrl ("login/" ++ model.name) 
                    , load "traffic.html"
                    ])

type Msg
    = ChangeName String
    | Submit

view : Model -> Html Msg
view model =
    div [ style [ ("text-align", "center") 
                , ("margin-top", "200px")
                , ("font-family", "Impact")
                , ("font-size", "50px")
                ]]
    [ h2 [] [ text "Name:" ]
    , Html.form [ onSubmit Submit ]
        [ input [ onInput ChangeName
                , placeholder "Dabson XD 420"
                , style [ ("font-size", "40px") 
                        , ("text-align", "center")
                        ]] []
        , br [] []
        , button [ style [ ("font-size", "40px") ] ] [text "Login!"]
        ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model = 
        WebSocket.keepAlive model.webSocketUrl 
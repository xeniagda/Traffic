module Base exposing (..)

import Time exposing (Time)
import Mouse
import Keyboard
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as P
import Window

zoomFactor = 1.2

carHeight : Float
carHeight =
    0.8


carWidth : Float
carWidth =
    carHeight * 2


pAdd : Position -> Position -> Position
pAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }

(!!) : List a -> Int -> Maybe a
(!!) lst n =
    List.head <| List.drop n lst

infixr 0 !!


type alias Controls = 
    { up      : Int
    , left    : Int
    , right   : Int
    , break   : Int
    , back    : Int
    , carUp    : Int
    , carDown  : Int
    , carFree  : Int
    , zoomIn  : Int
    , zoomOut : Int
    , remove : Int
    }

type alias Position =
    { x : Float, y : Float }

type alias Car =
    { name : String
    , img : String
    , pos : Position
    , rot : Float
    , speed : Float -- How many pixels forwards the car should move every second in the direction it's facing
    , accel : Float
    , steering : Float
    , crashed : Bool
    , handBreaks : Bool
    , breakStrength : Float
    , fade : Float
    , controlledBy : Maybe String
    }

type alias TrafficLight =
    { greenLeft : Float
    , offset : Position
    }

type alias Road =
    { start : Position
    , end : Position
    , connectedTo : List Int
    , trafficLight : Maybe TrafficLight
    , width : Float
    }


type alias Traffic =
    { cars : List Car
    , roads : List Road
    , ip : String
    }

getImg : Car -> String
getImg car =
    (if car.crashed then car.img ++ "Trasig" else car.img) ++ ".png"

getTrafficLightPath : TrafficLight -> String
getTrafficLightPath light =
    if light.greenLeft <= 0 then
        "Textures/TrafficLights/Light_red.png"
    else if light.greenLeft < 1 then
        "Textures/TrafficLights/Light_yellow.png"
    else
        "Textures/TrafficLights/Light_green.png"

decodeTraffic : Decoder Traffic
decodeTraffic =
    P.decode Traffic
        |> P.required "cars" decodeCars
        |> P.required "roads" decodeRoads
        |> P.required "ip" Decode.string


decodeCars : Decoder (List Car)
decodeCars =
    Decode.list
        (P.decode Car
            |> P.required "name" Decode.string
            |> P.required "img" Decode.string
            |> P.required "pos" decodePosition
            |> P.required "rot" Decode.float
            |> P.required "speed" Decode.float
            |> P.required "accel" Decode.float
            |> P.required "steering" Decode.float
            |> P.required "crashed" Decode.bool
            |> P.required "hand_breaks" Decode.bool
            |> P.required "break_strength" Decode.float
            |> P.optional "fade" Decode.float 1
            |> P.custom (Decode.maybe (Decode.field "controlled_by" Decode.string))
        )


decodeRoads : Decoder (List Road)
decodeRoads =
    Decode.list
        (P.decode Road
            |> P.required "start" decodePosition
            |> P.required "end" decodePosition
            |> P.required "connected_to" (Decode.list Decode.int)
            |> P.custom (Decode.maybe (Decode.field "traffic_light" decodeTrafficLight))
            |> P.optional "width" Decode.float 1
        )

decodeTrafficLight : Decoder TrafficLight
decodeTrafficLight =
    P.decode TrafficLight
        |> P.required "green_left" Decode.float
        |> P.required "offset" decodePosition


decodePosition : Decoder Position
decodePosition =
    Decode.map2 (\x y -> { x = x, y = y }) (Decode.field "x" Decode.float) (Decode.field "y" Decode.float)


type alias Model =
    { cars : List Car
    , roads : List Road
    , err : Maybe String
    , size : Maybe Position
    , lasttime : Maybe Time
    , scroll : Position -- Upper left corner
    , lastMouse : Maybe Position
    , renderScale : Float
    , webSocketUrl : String
    , msg : String -- Remove!
    , ip : Maybe String
    , accelRate : Float
    , steerRate : Float
    , lastClickTime : Maybe Float
    , trackingCar : Maybe String
    , controls : Controls
    , isPolis : Bool
    }


type Msg
    = ServerSync String
    | Resize Window.Size
    | CheckSize
    | UpdateClient Time
    | FixScroll
    | MousePress Mouse.Position
    | MouseRelease Mouse.Position
    | MouseMove Mouse.Position
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | SetMsg String
    | SendWebSocketMsg


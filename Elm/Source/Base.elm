module Base exposing (..)

import Time exposing (Time)
import Mouse
import Keyboard
import Window
import Svg as S

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

type MenuState = In | Out
type alias Menu =
    { radius : Float
    , height : Float
    , rotation : Float
    , state : MenuState
    , buttons : List MenuButton
    }

type alias MenuButton =
    { image : String
    , message : Msg
    }

defaultMenu =
    { radius = 30
    , height = 55
    , rotation = 0
    , state = In
    , buttons = []
    }

generateMenuButtons : List String -> List MenuButton
generateMenuButtons perms =
       (if List.member "place" perms then [ MenuButton "AddCar" <| AddCarClicked False] else [])
    ++ (if List.member "police" perms then [ MenuButton "AddPolice" <| AddCarClicked True] else [])
    ++ (if List.member "build" perms then [ MenuButton "AddRoad" AddRoadClicked] else [])


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
    , place : Int
    }

type alias Position =
    { x : Float, y : Float }

type alias ProtoCar =
    { pos : Position
    , img : String
    , isPolice : Bool
    }

toCar : ProtoCar -> Car
toCar prot =
    { name = "_"
    , img = prot.img
    , pos = prot.pos
    , rot = 0
    , speed = 0
    , accel = 0
    , steering = 0
    , crashed = False
    , handBreaks = False
    , breakStrength = 0.2
    , fade = 0.5
    , controlledBy = Nothing
    }

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
    , perms : List String
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

    , currentDragCar : Maybe ProtoCar

    , buildingRoad : Bool
    , buildingRoadStart : Maybe Position

    , menu : Menu
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
    | MenuBallClicked
    | AddCarClicked Bool
    | AddRoadClicked


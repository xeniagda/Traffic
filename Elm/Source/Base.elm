module Base exposing (..)

import Time exposing (Time)
import Mouse
import Keyboard
import Window
import Svg as S

nameLengthMax = 20

zoomFactor : Float
zoomFactor = 1.2

carHeight : Float
carHeight = 0.8

carWidth = carHeight * 2


type alias Position =
    { x : Float, y : Float }

pAdd : Position -> Position -> Position
pAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }

pRound : Position -> Position
pRound pos =
    { x = toFloat <| round pos.x, y = toFloat <| round pos.y }


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
    , buttons = [MenuButton "Info" <| OpenPopup <| InfoPopup True]
    }

generateMenuButtons : Traffic -> List MenuButton
generateMenuButtons traffic =
    (
        if traffic.loggedIn then
            let perms = traffic.perms
            in (if List.member "place" perms then [ MenuButton "AddCar" <| AddCarClicked False ] else [])
            ++ (if List.member "police" perms then [ MenuButton "AddPolice" <| AddCarClicked True ] else [])
            ++ (if List.member "build" perms then 
                [ MenuButton "AddRoad" AddRoadClicked
                , MenuButton "RemoveRoad" <| SelectStateChange RemoveSelecting
                , MenuButton "FlipRoad" <| SelectStateChange FlipSelecting 
                , MenuButton "CombineRoad" <| SelectStateChange CombineSelecting 
                , MenuButton "AddLight" <| SelectStateChange <| LightSelecting AddLight
                , MenuButton "FlipLight" <| SelectStateChange <| LightSelecting FlipLight
                , MenuButton "RemoveLight" <| SelectStateChange <| LightSelecting RemoveLight
                , MenuButton "CombineLight" <| SelectStateChange IntersectionMakeSelecting
                , MenuButton "Hide" <| SelectStateChange HideSelecting 
                , MenuButton "Show" ShowRoadClicked 
                ] else [])
            ++ (if List.member "command" perms then [ MenuButton "Cmd" <| OpenPopup <| CommandPopup "" ] else [])
        else
            [ MenuButton "login" <| OpenPopup <| LoginPopup ""
            ]
    ) 
    ++  [ MenuButton "Info" <| OpenPopup <| InfoPopup True 
        , MenuButton "TrackCar" <| SelectStateChange TrackSelecting
        ]


getClosestRoad : Position -> List Road -> Maybe Road
getClosestRoad pos roads =
    let dist road = 
        let positions = [road.end]
            lengths = List.map (\roadPos -> Tuple.first <| toPolar (roadPos.x - pos.x, roadPos.y - pos.y)) positions
        in List.sum lengths / 1000 + (Maybe.withDefault 10000 <| List.minimum <| lengths)
    in List.head <| List.sortBy dist roads

getClosestCar : Position -> List Car -> Maybe Car
getClosestCar pos car =
    let dist car =
        Tuple.first <| toPolar (pos.x - car.pos.x, pos.y - car.pos.y)
    in List.head <| List.filter (\car -> dist car < 5) <| List.sortBy dist car

id2idx : Model -> String -> Maybe Int
id2idx model id =
    List.head 
        <| List.filterMap (\(idx, road) -> if road.id == id then Just idx else Nothing)
        <| List.indexedMap (,) model.roads


type alias Controls = 
    { up : Int
    , left : Int
    , right : Int
    , break : Int
    , back : Int
    , carUp : Int
    , carDown : Int
    , free : Int
    , zoomIn : Int
    , zoomOut : Int
    , remove : Int
    , place : Int
    , snap : Int
    , hide : Int
    , help : Int
    }

type alias Car =
    { name : String
    , img : String
    , pos : Position
    , rot : Float
    , speed : Float
    , accel : Float
    , size : Float
    , steering : Float
    , crashed : Bool
    , handBreaks : Bool
    , breakStrength : Float
    , fade : Float
    , police : Bool
    , controlledBy : Maybe String
    }

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
    , size = 1
    , steering = 0
    , crashed = False
    , handBreaks = False
    , breakStrength = 0.2
    , fade = 0.5
    , police = prot.isPolice
    , controlledBy = Nothing
    }

type alias TrafficLight =
    { greenLeft : Float
    , offset : Float
    , at : Float
    }

type alias Road =
    { id : String
    , start : Position
    , end : Position
    , connectedTo : List String
    , trafficLight : Maybe TrafficLight
    , width : Float
    }

type alias User =
    { ip : String
    , name : String}

type alias Traffic =
    { cars : List Car
    , roads : List Road
    , ip : String
    , perms : List String
    , loggedIn : Bool
    , others : List User
    }

getImg : Car -> String
getImg car =
    (if car.crashed then car.img ++ "Trasig" else car.img) ++ ".png"

getTrafficLightPath : TrafficLight -> String
getTrafficLightPath light =
    if light.greenLeft <= 0 then
        "Textures/TrafficLights/Light_red.png"
    else if light.greenLeft < 2 then
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
    , dragMouse : Maybe Position
    , mouse : Maybe Position
    , renderScale : Float
    , webSocketUrl : String
    , ip : Maybe String
    , accelRate : Float
    , steerRate : Float
    , lastClickTime : Maybe Float
    , trackingCar : Maybe String
    , controls : Controls
    , others : List User

    , currentDragCar : Maybe ProtoCar

    , hiddenRoads : List String

    , buildingRoad : Bool
    , snap : Bool
    , buildingRoadStart : Maybe Position

    , selectState : SelectState
    , currentSelectedRoad : Maybe Road
    , otherRoad : Maybe Road

    , cmdLog : List String
    
    , popup : Popup
    , menu : Menu
    }

type Popup
    = NoPopup
    | LoginPopup String
    | InfoPopup Bool
    | CommandPopup String

type SelectState 
    = NotSelecting
    | CombineSelecting
    | RemoveSelecting
    | FlipSelecting
    | HideSelecting
    | LightSelecting LightState
    | IntersectionMakeSelecting
    | TrackSelecting

type LightState = AddLight | RemoveLight | FlipLight

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
    | MenuBallClicked
    | MenuButtonClicked Msg
    | AddCarClicked Bool
    | AddRoadClicked
    | SelectStateChange SelectState
    | ShowRoadClicked
    | OpenPopup Popup
    | ClosePopup
    | InfoToggleDebug
    | UpdateText String
    | SendCommand
    | ClearLogs
    | Login


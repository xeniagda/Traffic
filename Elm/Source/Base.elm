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
    , hotKey : Maybe (List String)
    , message : Msg
    }

defaultMenu =
    { radius = 30
    , height = 55
    , rotation = 0
    , state = In
    , buttons = [MenuButton "Info" Nothing <| OpenPopup <| InfoPopup True]
    }

generateMenuButtons : Traffic -> List MenuButton
generateMenuButtons traffic =
    (
        if traffic.loggedIn then
            let perms = traffic.perms
            in (if List.member "place" perms then [ MenuButton "AddCar" (Just ["C"]) <| AddCarClicked False ] else [])
            ++ (if List.member "police" perms then [ MenuButton "AddPolice" (Just ["P"]) <| AddCarClicked True ] else [])
            ++ (if List.member "build" perms then 
                [ MenuButton "AddRoad" (Just ["R", "M"]) AddRoadClicked
                , MenuButton "RemoveRoad" (Just ["R", "X"]) <| SelectStateChange RemoveSelecting
                , MenuButton "FlipRoad" (Just ["R", "F"]) <| SelectStateChange FlipSelecting 
                , MenuButton "CombineRoad" (Just ["R", "C"]) <| SelectStateChange CombineSelecting 
                , MenuButton "AddLight" (Just ["L", "M"]) <| SelectStateChange <| LightSelecting AddLight
                , MenuButton "FlipLight" (Just ["L", "F"]) <| SelectStateChange <| LightSelecting FlipLight
                , MenuButton "RemoveLight" (Just ["L", "X"]) <| SelectStateChange <| LightSelecting RemoveLight
                , MenuButton "CombineLight" (Just ["L", "C"]) <| SelectStateChange IntersectionMakeSelecting
                , MenuButton "Hide" (Just ["R", "H"]) <| SelectStateChange HideSelecting 
                , MenuButton "Show" (Just ["R", "CONTROL", "H"]) ShowRoadClicked 
                ] else [])
            ++ (if List.member "command" perms then [ MenuButton "Cmd" Nothing <| OpenPopup <| CommandPopup "" ] else [])
        else
            [ MenuButton "login" Nothing <| OpenPopup <| LoginPopup ""
            ]
    ) 
    ++  [ MenuButton "Info" Nothing <| OpenPopup <| InfoPopup True 
        , MenuButton "TrackCar" (Just ["T"]) <| SelectStateChange TrackSelecting
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
    { up : String
    , left : String
    , right : String
    , break : String
    , back : String
    , carUp : String
    , carDown : String
    , free : String
    , zoomIn : String
    , zoomOut : String
    , remove : String
    , place : String
    , snap : String
    , hide : String
    , help : String
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

    , keyCombo : List String

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
    | KeyCodeDown Keyboard.KeyCode
    | KeyCodeUp Keyboard.KeyCode
    | KeyDown String
    | KeyUp String
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


-- Stolen and modified from http://stackoverflow.com/a/23377822/1753929
keyboardMap = [
  "", -- [0]
  "", -- [1]
  "", -- [2]
  "CANCEL", -- [3]
  "", -- [4]
  "", -- [5]
  "HELP", -- [6]
  "", -- [7]
  "BACK_SPACE", -- [8]
  "TAB", -- [9]
  "", -- [10]
  "", -- [11]
  "CLEAR", -- [12]
  "ENTER", -- [13]
  "ENTER_SPECIAL", -- [14]
  "", -- [15]
  "SHIFT", -- [16]
  "CONTROL", -- [17]
  "ALT", -- [18]
  "PAUSE", -- [19]
  "CAPS_LOCK", -- [20]
  "KANA", -- [21]
  "EISU", -- [22]
  "JUNJA", -- [23]
  "FINAL", -- [24]
  "HANJA", -- [25]
  "", -- [26]
  "ESCAPE", -- [27]
  "CONVERT", -- [28]
  "NONCONVERT", -- [29]
  "ACCEPT", -- [30]
  "MODECHANGE", -- [31]
  "SPACE", -- [32]
  "PAGE_UP", -- [33]
  "PAGE_DOWN", -- [34]
  "END", -- [35]
  "HOME", -- [36]
  "LEFT", -- [37]
  "UP", -- [38]
  "RIGHT", -- [39]
  "DOWN", -- [40]
  "SELECT", -- [41]
  "PRINT", -- [42]
  "EXECUTE", -- [43]
  "PRINTSCREEN", -- [44]
  "INSERT", -- [45]
  "DELETE", -- [46]
  "", -- [47]
  "0", -- [48]
  "1", -- [49]
  "2", -- [50]
  "3", -- [51]
  "4", -- [52]
  "5", -- [53]
  "6", -- [54]
  "7", -- [55]
  "8", -- [56]
  "9", -- [57]
  "COLON", -- [58]
  "SEMICOLON", -- [59]
  "LESS_THAN", -- [60]
  "EQUALS", -- [61]
  "GREATER_THAN", -- [62]
  "QUESTION_MARK", -- [63]
  "AT", -- [64]
  "A", -- [65]
  "B", -- [66]
  "C", -- [67]
  "D", -- [68]
  "E", -- [69]
  "F", -- [70]
  "G", -- [71]
  "H", -- [72]
  "I", -- [73]
  "J", -- [74]
  "K", -- [75]
  "L", -- [76]
  "M", -- [77]
  "N", -- [78]
  "O", -- [79]
  "P", -- [80]
  "Q", -- [81]
  "R", -- [82]
  "S", -- [83]
  "T", -- [84]
  "U", -- [85]
  "V", -- [86]
  "W", -- [87]
  "X", -- [88]
  "Y", -- [89]
  "Z", -- [90]
  "OS_KEY", -- [91] Windows Key (Windows) or Command Key (Mac)
  "", -- [92]
  "CONTEXT_MENU", -- [93]
  "", -- [94]
  "SLEEP", -- [95]
  "NUMPAD0", -- [96]
  "NUMPAD1", -- [97]
  "NUMPAD2", -- [98]
  "NUMPAD3", -- [99]
  "NUMPAD4", -- [100]
  "NUMPAD5", -- [101]
  "NUMPAD6", -- [102]
  "NUMPAD7", -- [103]
  "NUMPAD8", -- [104]
  "NUMPAD9", -- [105]
  "MULTIPLY", -- [106]
  "ADD", -- [107]
  "SEPARATOR", -- [108]
  "SUBTRACT", -- [109]
  "DECIMAL", -- [110]
  "DIVIDE", -- [111]
  "F1", -- [112]
  "F2", -- [113]
  "F3", -- [114]
  "F4", -- [115]
  "F5", -- [116]
  "F6", -- [117]
  "F7", -- [118]
  "F8", -- [119]
  "F9", -- [120]
  "F10", -- [121]
  "F11", -- [122]
  "F12", -- [123]
  "F13", -- [124]
  "F14", -- [125]
  "F15", -- [126]
  "F16", -- [127]
  "F17", -- [128]
  "F18", -- [129]
  "F19", -- [130]
  "F20", -- [131]
  "F21", -- [132]
  "F22", -- [133]
  "F23", -- [134]
  "F24", -- [135]
  "", -- [136]
  "", -- [137]
  "", -- [138]
  "", -- [139]
  "", -- [140]
  "", -- [141]
  "", -- [142]
  "", -- [143]
  "NUM_LOCK", -- [144]
  "SCROLL_LOCK", -- [145]
  "WIN_OEM_FJ_JISHO", -- [146]
  "WIN_OEM_FJ_MASSHOU", -- [147]
  "WIN_OEM_FJ_TOUROKU", -- [148]
  "WIN_OEM_FJ_LOYA", -- [149]
  "WIN_OEM_FJ_ROYA", -- [150]
  "", -- [151]
  "", -- [152]
  "", -- [153]
  "", -- [154]
  "", -- [155]
  "", -- [156]
  "", -- [157]
  "", -- [158]
  "", -- [159]
  "CIRCUMFLEX", -- [160]
  "EXCLAMATION", -- [161]
  "DOUBLE_QUOTE", -- [162]
  "HASH", -- [163]
  "DOLLAR", -- [164]
  "PERCENT", -- [165]
  "AMPERSAND", -- [166]
  "UNDERSCORE", -- [167]
  "OPEN_PAREN", -- [168]
  "CLOSE_PAREN", -- [169]
  "ASTERISK", -- [170]
  "PLUS", -- [171]
  "PIPE", -- [172]
  "HYPHEN_MINUS", -- [173]
  "OPEN_CURLY_BRACKET", -- [174]
  "CLOSE_CURLY_BRACKET", -- [175]
  "TILDE", -- [176]
  "", -- [177]
  "", -- [178]
  "", -- [179]
  "", -- [180]
  "VOLUME_MUTE", -- [181]
  "VOLUME_DOWN", -- [182]
  "VOLUME_UP", -- [183]
  "", -- [184]
  "", -- [185]
  "SEMICOLON", -- [186]
  "EQUALS", -- [187]
  "COMMA", -- [188]
  "MINUS", -- [189]
  "PERIOD", -- [190]
  "SLASH", -- [191]
  "BACK_QUOTE", -- [192]
  "", -- [193]
  "", -- [194]
  "", -- [195]
  "", -- [196]
  "", -- [197]
  "", -- [198]
  "", -- [199]
  "", -- [200]
  "", -- [201]
  "", -- [202]
  "", -- [203]
  "", -- [204]
  "", -- [205]
  "", -- [206]
  "", -- [207]
  "", -- [208]
  "", -- [209]
  "", -- [210]
  "", -- [211]
  "", -- [212]
  "", -- [213]
  "", -- [214]
  "", -- [215]
  "", -- [216]
  "", -- [217]
  "", -- [218]
  "OPEN_BRACKET", -- [219]
  "BACK_SLASH", -- [220]
  "CLOSE_BRACKET", -- [221]
  "QUOTE", -- [222]
  "", -- [223]
  "META", -- [224]
  "ALTGR", -- [225]
  "", -- [226]
  "WIN_ICO_HELP", -- [227]
  "WIN_ICO_00", -- [228]
  "", -- [229]
  "WIN_ICO_CLEAR", -- [230]
  "", -- [231]
  "", -- [232]
  "WIN_OEM_RESET", -- [233]
  "WIN_OEM_JUMP", -- [234]
  "WIN_OEM_PA1", -- [235]
  "WIN_OEM_PA2", -- [236]
  "WIN_OEM_PA3", -- [237]
  "WIN_OEM_WSCTRL", -- [238]
  "WIN_OEM_CUSEL", -- [239]
  "WIN_OEM_ATTN", -- [240]
  "WIN_OEM_FINISH", -- [241]
  "WIN_OEM_COPY", -- [242]
  "WIN_OEM_AUTO", -- [243]
  "WIN_OEM_ENLW", -- [244]
  "WIN_OEM_BACKTAB", -- [245]
  "ATTN", -- [246]
  "CRSEL", -- [247]
  "EXSEL", -- [248]
  "EREOF", -- [249]
  "PLAY", -- [250]
  "ZOOM", -- [251]
  "", -- [252]
  "PA1", -- [253]
  "WIN_OEM_CLEAR", -- [254]
  ""]

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline as P
import Svg as S
import Svg.Attributes as Sa
import Window
import Time exposing (Time)
import Mouse
import Keyboard
import WebSocket

import Debug

zoomFactor = 1.2
controlUp = 87      -- W
controlLeft = 65    -- A
controlRight = 68   -- D
controlBreak = 83   -- S
controlBack = 88    -- X
switchCarUp = 39    -- Right
switchCarDown = 37  -- Left
switchCarFree = 27  -- Escape

pAdd : Position -> Position -> Position
pAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }

(!!) : List a -> Int -> Maybe a
(!!) lst n =
    List.head <| List.drop n lst

infixr 0 !!

type alias Flags =
    { webSocketUrl : String
    }

main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Position =
    { x : Float, y : Float }


carHeight : Float
carHeight =
    0.8


carWidth : Float
carWidth =
    carHeight * 2


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
    , accel_rate : Float
    , steer_rate : Float
    , lastClickTime : Maybe Float
    , trackingCar : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model [] [] Nothing Nothing Nothing {x=0, y=0} Nothing 40 (Debug.log "Websocket url: " flags.webSocketUrl) "" Nothing 3 200 Nothing Nothing
    , Task.perform identity <| Task.succeed CheckSize
    )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerSync json ->
            let result = decodeString decodeTraffic json
            in case result of
                Ok traffic ->
                    ( { model | cars = traffic.cars, roads = traffic.roads, ip = Just traffic.ip, err = Nothing }, Task.perform identity <| Task.succeed FixScroll )
                Err error ->
                    ( { model | err = Just error }, Cmd.none )

        Resize size ->
            ( { model | size = Just { x = toFloat size.width, y = toFloat size.height } }, Cmd.none )

        CheckSize ->
            ( model, Task.perform Resize <| Window.size )

        UpdateClient time ->
            case model.lasttime of
                Nothing ->
                    ( { model | lasttime = Just time }, Cmd.none )

                Just lasttime ->
                    let
                        delta =
                            Time.inSeconds (time - lasttime)
                    in
                        ( { model
                            | lasttime = Just time
                            , cars =
                                List.map (\car ->
                                        { car
                                            | pos =
                                                pAdd car.pos <| (\( x, y ) -> { x = x, y = y }) <| fromPolar ( car.speed * delta, degrees car.rot )
                                            , speed = (if car.handBreaks then car.speed * (car.breakStrength ^ delta) else car.speed) + car.accel * delta
                                            , rot = car.rot + car.steering * delta
                                            , steering =
                                                case model.err of
                                                    Just _  -> car.steering / (2 ^ delta)
                                                    Nothing -> car.steering
                                        }
                                    )
                                    model.cars
                          }
                        , Task.perform identity <| Task.succeed FixScroll
                        )

        FixScroll ->
            ( {model | scroll =
                case (model.size, model.trackingCar) of
                    (Just size, Just carName) ->
                        let car_ = List.head <| List.filter (\car -> car.name == carName) model.cars
                        in case car_ of
                            Just car -> { x = -car.pos.x * model.renderScale + size.x / 2, y = -car.pos.y * model.renderScale + size.y / 2 }
                            Nothing -> model.scroll
                    _ -> model.scroll
                        
              }, Cmd.none )
        MouseRelease _ ->
            ( {model | lastMouse = Nothing, lastClickTime = model.lasttime}, Cmd.none )

        MousePress pos ->
            case model.lasttime of
                Just lasttime ->
                    case model.lastClickTime of 
                        Just lastClickTime ->
                            if lasttime - lastClickTime > 500 then
                                ( {model | lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )
                            else
                                let x = (toFloat pos.x - model.scroll.x) / model.renderScale
                                    y = (toFloat pos.y - model.scroll.y) / model.renderScale
                                in ( model, WebSocket.send model.webSocketUrl <| "create/" ++ toString x ++ "/" ++ toString y )
                        Nothing -> ( {model | lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )

                Nothing -> ( {model | lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )

        MouseMove pos ->
            let track = 
                case model.trackingCar of
                    Nothing -> True
                    Just name -> List.length (List.filter (\car -> car.name == name) model.cars) == 0
            in if track then
                case model.lastMouse of
                    Just mousePos ->
                        let delta = {x = toFloat pos.x - mousePos.x, y = toFloat pos.y - mousePos.y}
                        in ( {model | scroll = {x = model.scroll.x + delta.x, y = model.scroll.y + delta.y}, lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )
                    Nothing ->
                        (model, Cmd.none)
            else ( model, Cmd.none )

        KeyDown key ->
            case model.ip of
                Just myIp ->
                    if key == 187 then -- Plus key
                        ( {model 
                            | renderScale = model.renderScale * zoomFactor
                            , scroll = 
                                case model.size of
                                    Just size -> 
                                        {x = (model.scroll.x - size.x / 2) * zoomFactor + size.x / 2, y = (model.scroll.y - size.y / 2) * zoomFactor + size.y / 2}
                                    Nothing ->
                                        model.scroll
                        }, Cmd.none )
                    else if key == 189 then -- Minus key
                        ( {model 
                            | renderScale = model.renderScale / zoomFactor
                            , scroll = 
                                case model.size of
                                    Just size -> 
                                        {x = (model.scroll.x - size.x / 2) / zoomFactor + size.x / 2, y = (model.scroll.y - size.y / 2) / zoomFactor + size.y / 2}
                                    Nothing ->
                                        model.scroll
                        }, Cmd.none )
                    else if key == controlBreak then -- k
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = True, accel = 0}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "breaks" )
                    else if key == controlUp then -- i
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = False, accel = model.accel_rate}
                                    Nothing -> car
                            ) model.cars},
                            WebSocket.send model.webSocketUrl ( "accel/" ++ (toString model.accel_rate) ) )
                    else if key == controlBack then -- m
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = False, accel = negate model.accel_rate}
                                    Nothing -> car
                            ) model.cars},
                            WebSocket.send model.webSocketUrl ( "accel/" ++ (toString <| negate model.accel_rate) ) )
                    else if key == controlLeft then -- j
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | steering = negate model.steer_rate}
                                    Nothing -> car
                            ) model.cars},
                            WebSocket.send model.webSocketUrl ( "steer/" ++ (toString <| negate model.steer_rate)) )
                    else if key == controlRight then -- l
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | steering = model.steer_rate}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl ( "steer/" ++ (toString model.steer_rate)))
                    else if key == switchCarFree then
                        ( {model | trackingCar = Nothing }, Cmd.none )
                    else if key == switchCarUp || key == switchCarDown then
                        ( { model | 
                            trackingCar =
                                let controlledByMe =
                                    List.sortWith (\car1 car2 -> compare car1.pos.x car2.pos.x) <|
                                        case model.ip of
                                            Just ip -> List.filter (\car -> 
                                                case car.controlledBy of
                                                    Just c -> c == ip
                                                    Nothing -> False) model.cars
                                            Nothing -> []
                                in case model.trackingCar of
                                    Nothing -> Maybe.map .name <| List.head controlledByMe
                                    Just trackName ->
                                        let track = List.head <| List.filter (\(idx, car) -> car.name == trackName) (List.indexedMap (,) controlledByMe)
                                        in case track of
                                            Just (idx, car) -> 
                                                let res = 
                                                    if key == switchCarUp then 
                                                        Maybe.map .name <| List.head <| List.drop (idx+1) <| Debug.log "controlled" controlledByMe
                                                    else
                                                        if idx == 0 then Nothing else Maybe.map .name <| List.head <| List.drop (idx-1) <| Debug.log "controlled" controlledByMe
                                                in case res of
                                                    Just x -> Just x
                                                    Nothing -> 
                                                        if key == switchCarUp then 
                                                            Maybe.map .name <| List.head controlledByMe
                                                        else
                                                            Maybe.map .name <| List.head <| List.reverse controlledByMe
                                            Nothing -> Maybe.map .name <| List.head controlledByMe
                          }
                          , Cmd.none )
                    else (always (model, Cmd.none)) <| Debug.log "Key Down" key
                Nothing -> ( model, Cmd.none )

        KeyUp key ->
            case model.ip of
                Just myIp ->
                    if key == controlBreak then -- k
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = False}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "no_breaks" )
                    else if key == controlUp || key == controlBack then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | accel = 0}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "accel/0" )
                    else if key == controlLeft || key == controlRight then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | steering = 0}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "steer/0" )
                    else (always (model, Cmd.none)) <| Debug.log "Key Up" key
                Nothing -> ( model, Cmd.none )

        SetMsg msg ->
            ( {model | msg = msg}, Cmd.none )

        SendWebSocketMsg ->
            ( model, WebSocket.send model.webSocketUrl model.msg )

view : Model -> Html Msg
view model =
    div [style [("margin", "0px")]] <|
        [ -- Generate svg!
          S.svg
            [ Sa.width <| Maybe.withDefault "10" <| Maybe.map (toString << .x) model.size
            , Sa.height <| Maybe.withDefault "10" <| Maybe.map (toString << .y) model.size
            , Sa.style "background: #004400"
            ]
            (let
                lines =
                    (case model.size of
                        Just pos ->
                            (List.map (\x ->
                                    S.line
                                        [ Sa.x1 <| toString <| floor <| model.renderScale * toFloat x + toFloat (round model.scroll.x % round model.renderScale)
                                        , Sa.y1 "0"
                                        , Sa.x2 <| toString <| floor <| model.renderScale * toFloat x + toFloat (round model.scroll.x % round model.renderScale)
                                        , Sa.y2 <| toString <| pos.y
                                        , Sa.stroke "black"
                                        , Sa.strokeWidth "0.2"
                                        ]
                                        []
                                )
                             <|
                                List.range 0 <|
                                    floor <|
                                        pos.x / model.renderScale
                            )
                                ++ (List.map
                                        (\y ->
                                            S.line
                                                [ Sa.x1 "0"
                                                , Sa.y1 <| toString <| floor <| model.renderScale * toFloat y + toFloat (round model.scroll.y % round model.renderScale)
                                                , Sa.x2 <| toString <| pos.x
                                                , Sa.y2 <| toString <| floor <| model.renderScale * toFloat y + toFloat (round model.scroll.y % round model.renderScale)
                                                , Sa.stroke "black"
                                                , Sa.strokeWidth "0.2"
                                                ]
                                                []
                                        )
                                    <|
                                        List.range 0 <|
                                            floor <|
                                                pos.y / model.renderScale
                                   )

                        Nothing ->
                            []
                    )

                roads =
                    List.map (\road ->
                            S.line
                                [ Sa.x1 <| toString <| floor <| (road.start.x * model.renderScale) + model.scroll.x
                                , Sa.y1 <| toString <| floor <| (road.start.y * model.renderScale) + model.scroll.y
                                , Sa.x2 <| toString <| floor <| (road.end.x * model.renderScale) + model.scroll.x
                                , Sa.y2 <| toString <| floor <| (road.end.y * model.renderScale) + model.scroll.y
                                , Sa.strokeWidth <| toString <| model.renderScale * road.width + 2
                                , Sa.stroke "gray"
                                ] []
                        )
                        model.roads
                roadCaps =
                    List.concatMap (\road ->
                            List.concatMap (\idx ->
                                    let maybeOtherRoad = model.roads !! idx
                                    in case maybeOtherRoad of
                                        Just otherRoad ->
                                            let roadRot = (Tuple.second <| toPolar (road.end.x - road.start.x, road.end.y - road.start.y))
                                                otherRoadRot = (Tuple.second <| toPolar (otherRoad.end.x - otherRoad.start.x, otherRoad.end.y - otherRoad.start.y))
                                                roadEdge1x = (road.end.x + (Tuple.first <| fromPolar (road.width / 2, roadRot - pi / 2))) * model.renderScale + model.scroll.x
                                                roadEdge1y = (road.end.y + (Tuple.second <| fromPolar (road.width / 2, roadRot - pi / 2))) * model.renderScale + model.scroll.y
                                                otherRoadEdge1x = (otherRoad.start.x + (Tuple.first <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * model.renderScale + model.scroll.x
                                                otherRoadEdge1y = (otherRoad.start.y + (Tuple.second <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * model.renderScale + model.scroll.y
                                                roadEdge2x = (road.end.x - (Tuple.first <| fromPolar (road.width / 2, roadRot - pi / 2))) * model.renderScale + model.scroll.x
                                                roadEdge2y = (road.end.y - (Tuple.second <| fromPolar (road.width / 2, roadRot - pi / 2))) * model.renderScale + model.scroll.y
                                                otherRoadEdge2x = (otherRoad.start.x - (Tuple.first <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * model.renderScale + model.scroll.x
                                                otherRoadEdge2y = (otherRoad.start.y - (Tuple.second <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * model.renderScale + model.scroll.y
                                            in [ S.polygon
                                                    [ Sa.points <| (toString roadEdge1x) ++ " " ++ (toString roadEdge1y)
                                                         ++ "," ++ (toString otherRoadEdge1x) ++ " " ++ (toString otherRoadEdge1y)
                                                         ++ "," ++ (toString otherRoadEdge2x) ++ " " ++ (toString otherRoadEdge2y)
                                                         ++ "," ++ (toString roadEdge2x) ++ " " ++ (toString roadEdge2y)
                                                    , Sa.style "fill:gray;stroke:gray;stroke-width:1"
                                                    ] []

                                            ]
                                        Nothing ->
                                            []
                                ) road.connectedTo
                        ) model.roads
                roadLines =
                    List.map (\road ->
                            S.line
                                [ Sa.x1 <| toString <| floor <| road.start.x * model.renderScale + model.scroll.x
                                , Sa.y1 <| toString <| floor <| road.start.y * model.renderScale + model.scroll.y
                                , Sa.x2 <| toString <| floor <| road.end.x * model.renderScale + model.scroll.x
                                , Sa.y2 <| toString <| floor <| road.end.y * model.renderScale + model.scroll.y
                                , Sa.strokeWidth <| toString <| model.renderScale / 6
                                , Sa.stroke "yellow"
                                , Sa.strokeDasharray <| (toString <| model.renderScale / 6) ++ ", " ++ (toString <| model.renderScale / 3)
                                ]
                                []
                        ) model.roads
                cars =
                    List.map (\car ->
                            S.image
                              [ Sa.x <| toString <| floor <| model.scroll.x + car.pos.x * model.renderScale - carWidth / 2 * model.renderScale
                              , Sa.y <| toString <| floor <| model.scroll.y + car.pos.y * model.renderScale - carHeight / 2 * model.renderScale
                              , Sa.width <| toString <| carWidth * model.renderScale
                              , Sa.height <| toString <| carHeight * model.renderScale
                              , Sa.xlinkHref <| "Textures/Cars/" ++ getImg car
                              , Sa.opacity <| toString car.fade
                              , Sa.transform <|
                                  "rotate("
                                      ++ (toString car.rot)
                                      ++ " "
                                      ++ (toString <| car.pos.x * model.renderScale + model.scroll.x)
                                      ++ " "
                                      ++ (toString <| car.pos.y * model.renderScale + model.scroll.y)
                                      ++ ")"
                              ]
                              []

                        )
                        model.cars
                err =
                    case model.err of
                        Just err ->
                            [S.text_
                                [ Sa.x "0"
                                , Sa.y "20"
                                , Sa.fill "red"] [S.text <| toString err]]
                        Nothing -> []
                trafficLights =
                    List.concatMap (\road ->
                        case road.trafficLight of
                            Just light ->
                                let roadDelta = {x = road.end.x - road.start.x, y = road.end.y - road.start.y}
                                    roadRotation = (Tuple.second <| toPolar (roadDelta.x, roadDelta.y)) / pi * 180 + 90
                                in [S.image
                                    [ Sa.x <| toString <| (road.end.x + light.offset.x - 0.5) * model.renderScale + model.scroll.x
                                    , Sa.y <| toString <| (road.end.y + light.offset.y - 0.5) * model.renderScale + model.scroll.y
                                    , Sa.width <| toString model.renderScale
                                    , Sa.height <| toString model.renderScale
                                    , Sa.transform <|
                                        "rotate(" ++ (toString roadRotation) ++
                                              " " ++ (toString <| (road.end.x + light.offset.x) * model.renderScale + model.scroll.x) ++
                                              " " ++ (toString <| (road.end.y + light.offset.y) * model.renderScale + model.scroll.y) ++
                                              ")"
                                    , Sa.xlinkHref <| getTrafficLightPath light
                                    ] []
                                ]
                            Nothing -> []
                    ) model.roads
             in
                lines ++ roads ++ roadCaps ++ roadLines ++ cars ++ trafficLights
            )
        ] ++
        [ pre [] [text "Keys:"]
        , div [style [("margin-left", "2em")]] (
            [ pre [] [text "+ Zoom in"]
            , pre [] [text "- Zoom out"]
            , pre [] [text "Double-click to make a car"]
            , pre [] [text "w Accelerate"]
            , pre [] [text "s Active breaks"]
            , pre [] [text "a/d Steer left/right"]
            , pre [] [text "x Drive backwards"]
            ] ++ (
                case model.ip of
                    Just ip -> [p [] [text <| "Your IP: " ++ ip]]
                    Nothing -> [p [] [text "No IP assigned."]]
            ) ++ (
            case model.err of
                Just err ->
                    [ p [style [("color", "red")]] [text <| "An error occured. Error was: " ++ toString err]
                    ]
                Nothing ->
                    []
        )
        )]



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Time.every (Time.second / 30) UpdateClient
        , Mouse.downs MousePress
        , Mouse.ups MouseRelease
        , Mouse.moves MouseMove
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , WebSocket.listen model.webSocketUrl ServerSync
        ]

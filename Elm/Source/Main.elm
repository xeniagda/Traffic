module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Svg as S
import Svg.Attributes as Sa
import Window
import Time exposing (Time)
import Mouse
import Keyboard
import WebSocket

import Base exposing (..)
import TrafficRenderer exposing (..)
import MenuRenderer exposing (renderMenu)
import JsonParser exposing (..)

import Debug

type alias Flags =
    { webSocketUrl : String
    , controls : Controls
    , isRubs : Bool
    }

main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }



init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model [] [] Nothing Nothing Nothing {x=0, y=0} Nothing 40 (Debug.log "Websocket url: " flags.webSocketUrl) "" Nothing 6 200 Nothing Nothing flags.controls (Debug.log "is rubs" flags.isRubs) Nothing defaultMenu
    , Task.perform identity <| Task.succeed CheckSize
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerSync json ->
            let result = decodeString decodeTraffic json
            in case result of
                Ok traffic ->
                    ( { model 
                        | cars = traffic.cars
                        , roads = traffic.roads
                        , ip = Just traffic.ip
                        , err = Nothing 
                        , menu = let menu = model.menu
                                 in { menu
                                    | buttons = generateMenuButtons traffic.perms
                                    }
                    }
                    , Task.perform identity <| Task.succeed FixScroll )
                Err error ->
                    ( { model 
                        | err = Just error 
                    }
                    , Cmd.none )

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
                            , menu = let menu = model.menu
                                     in { menu 
                                        | rotation = case menu.state of
                                            In -> if menu.rotation > 0 then menu.rotation - delta * 5 else 0
                                            Out -> if menu.rotation < 1 then menu.rotation + delta * 5 else 1
                                        }
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
            ( {model 
                | lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}
                , currentDragCar = Nothing}
            , case model.currentDragCar of
                Just car -> WebSocket.send model.webSocketUrl <| "create/" ++ (Http.encodeUri <| encode 0 <| encodeProtoCar car)
                Nothing -> Cmd.none
            )

        MouseMove pos ->
            let track =
                case model.trackingCar of
                    Nothing -> False
                    Just name -> List.length (List.filter (\car -> car.name == name) model.cars) > 0
            in if not track then
                case model.lastMouse of
                    Just mousePos ->
                        let delta = {x = toFloat pos.x - mousePos.x, y = toFloat pos.y - mousePos.y}
                        in ( {model | scroll = {x = model.scroll.x + delta.x, y = model.scroll.y + delta.y}, lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )
                    Nothing ->
                        case model.currentDragCar of
                            Just car ->
                                ( { model
                                | currentDragCar = Just {car | pos = {x = (toFloat pos.x - model.scroll.x) / model.renderScale, y = (toFloat pos.y - model.scroll.y) / model.renderScale}}
                                }, Cmd.none )
                            Nothing -> ( model, Cmd.none )
            else ( model, Cmd.none )

        KeyDown key ->
            if key == model.controls.zoomIn then -- Plus key
                ( {model
                    | renderScale = model.renderScale * zoomFactor
                    , scroll =
                        case model.size of
                            Just size ->
                                {x = (model.scroll.x - size.x / 2) * zoomFactor + size.x / 2, y = (model.scroll.y - size.y / 2) * zoomFactor + size.y / 2}
                            Nothing ->
                                model.scroll
                }, Cmd.none )
            else if key == model.controls.zoomOut then -- Minus key
                ( {model
                    | renderScale = model.renderScale / zoomFactor
                    , scroll =
                        case model.size of
                            Just size ->
                                {x = (model.scroll.x - size.x / 2) / zoomFactor + size.x / 2, y = (model.scroll.y - size.y / 2) / zoomFactor + size.y / 2}
                            Nothing ->
                                model.scroll
                }, Cmd.none )
            else case model.ip of
                Just myIp ->
                    if key == model.controls.remove then
                        ( model
                        ,
                            let distToScroll car = Tuple.first <| toPolar <| (car.pos.x - model.scroll.x, car.pos.y - model.scroll.y)
                                closestCar = List.head
                                    <| List.sortWith (\c1 c2 -> compare (distToScroll c2) (distToScroll c1) )
                                    <| List.filter (\c -> c.controlledBy == model.ip) model.cars
                            in case closestCar of
                                Just {name} -> WebSocket.send model.webSocketUrl <| "remove/" ++ name
                                Nothing -> Cmd.none
                        )
                    else if key == model.controls.break then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = True, accel = 0}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "breaks" )
                    else if key == model.controls.up then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = False, accel = model.accelRate}
                                    Nothing -> car
                            ) model.cars},
                            WebSocket.send model.webSocketUrl ( "accel/" ++ (toString model.accelRate) ) )
                    else if key == model.controls.back then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = False, accel = negate model.accelRate}
                                    Nothing -> car
                            ) model.cars},
                            WebSocket.send model.webSocketUrl ( "accel/" ++ (toString <| negate model.accelRate) ) )
                    else if key == model.controls.left then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | steering = negate model.steerRate}
                                    Nothing -> car
                            ) model.cars},
                            WebSocket.send model.webSocketUrl ( "steer/" ++ (toString <| negate model.steerRate)) )
                    else if key == model.controls.right then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | steering = model.steerRate}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl ( "steer/" ++ (toString model.steerRate)))
                    else if key == model.controls.carFree then
                        ( {model | trackingCar = Nothing }, Cmd.none )
                    else if key == model.controls.carUp || key == model.controls.carDown then
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
                                                    if key == model.controls.carUp then
                                                        Maybe.map .name <| List.head <| List.drop (idx+1) controlledByMe
                                                    else
                                                        if idx == 0 then Nothing else Maybe.map .name <| List.head <| List.drop (idx-1) <| controlledByMe
                                                in case res of
                                                    Just x -> Just x
                                                    Nothing ->
                                                        if key == model.controls.carUp then
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
                    if key == model.controls.break then -- k
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | handBreaks = False}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "no_breaks" )
                    else if key == model.controls.up || key == model.controls.back then
                        ( { model | cars = List.map (\car ->
                                case car.controlledBy of
                                    Just ip -> if ip /= myIp then car else
                                                  {car | accel = 0}
                                    Nothing -> car
                            ) model.cars},
                        WebSocket.send model.webSocketUrl "accel/0" )
                    else if key == model.controls.left || key == model.controls.right then
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

        MenuBallClicked ->
            ( { model
            | menu =
                let menu = model.menu
                in { menu | state = case model.menu.state of
                                                In -> Out
                                                Out -> In
            }
            }, Cmd.none )

        AddCarClicked ->
            ( { model 
            | currentDragCar = Just {pos = {x = 0, y = 0}, img = "Car1", isPolice = False}
            , menu = let menu = model.menu
                     in { menu | state = In }
            }, Cmd.none )

        AddPoliceClicked ->
            ( { model 
            | currentDragCar = Just {pos = {x = 0, y = 0}, img = "CarPolis", isPolice = True}
            , menu = let menu = model.menu
                     in { menu | state = In }
            }, Cmd.none )

view : Model -> Html Msg
view model =
    div [style [("margin", "0px")]] <|
        [ -- Generate svg!
          S.svg
            [ Sa.width <| Maybe.withDefault "10" <| Maybe.map (toString << .x) model.size
            , Sa.height <| Maybe.withDefault "10" <| Maybe.map (toString << .y) model.size
            , Sa.style "background: #227722"
            ]
            (let
                lines = renderBackgroundLines model
                roads = renderRoads model model.roads
                cars = renderCars model model.cars
                trafficLights = renderTrafficLights model model.roads
                menu = renderMenu model
             in
                lines ++ roads ++ cars ++ trafficLights ++ menu
            ++ (
                case model.currentDragCar of
                    Just car -> renderCars model [toCar car]
                    Nothing -> []
            ))
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

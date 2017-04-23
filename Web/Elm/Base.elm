module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as P
import Svg as S
import Svg.Attributes as Sa
import Window
import Time exposing (Time)
import Mouse


renderScale =
    40


pAdd : Position -> Position -> Position
pAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }

(!!) : List a -> Int -> Maybe a
(!!) lst n =
    List.head <| List.drop n lst

infixr 0 !!

main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Position =
    { x : Float, y : Float }




carHeight : Float
carHeight =
    25


carWidth : Float
carWidth =
    carHeight * 2


type alias Car =
    { name : String
    , img : String
    , pos : Position
    , rot : Float
    , speed : Float -- How many pixels forwards the car should move every second in the direction it's facing
    , steering : Float
    , crashed : Bool
    , fade : Float
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
    Decode.map2 (\cars roads -> { cars = cars, roads = roads })
        (Decode.field "cars" decodeCars)
        (Decode.field "roads" decodeRoads)


decodeCars : Decoder (List Car)
decodeCars =
    Decode.list
        (P.decode Car
            |> P.required "name" Decode.string
            |> P.required "img" Decode.string
            |> P.required "pos" decodePosition
            |> P.required "rot" Decode.float
            |> P.required "speed" Decode.float
            |> P.required "steering" Decode.float
            |> P.required "crashed" Decode.bool
            |> P.optional "fade" Decode.float 1
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
    , err : Maybe Http.Error
    , size : Maybe Position
    , lasttime : Maybe Time
    , scroll : Position -- Upper left corner
    , lastMouse : Maybe Position
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] [] Nothing Nothing Nothing {x=0, y=0} Nothing
    , Cmd.batch
        [ Task.perform identity <| Task.succeed Request
        , Task.perform identity <| Task.succeed CheckSize
        ]
    )


type Msg
    = Request
    | SetTrafficState (Result Http.Error Traffic)
    | Resize Window.Size
    | CheckSize
    | UpdateClient Time
    | UpdateServer Time
    | MousePress Mouse.Position
    | MouseRelease Mouse.Position
    | MouseMove Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTrafficState (Ok traffic) ->
            ( { model | cars = traffic.cars, roads = traffic.roads, err = Nothing }, Cmd.none )

        SetTrafficState (Err error) ->
            ( { model | err = Just error }, Cmd.none )

        Request ->
            ( model, Http.send SetTrafficState <| Http.get "/Cars" decodeTraffic )

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
                                            , rot = car.rot + car.steering * delta
                                            , speed =
                                                case model.err of
                                                    Just _  -> car.speed / (5 ^ delta)
                                                    Nothing -> car.speed
                                            , steering =
                                                case model.err of
                                                    Just _  -> car.steering / (2 ^ delta)
                                                    Nothing -> car.steering
                                        }
                                    )
                                    model.cars
                          }
                        , Cmd.none
                        )

        UpdateServer time ->
            ( model, Task.perform identity <| Task.succeed Request )

        MouseRelease _ ->
            ( {model | lastMouse = Nothing}, Cmd.none )

        MousePress pos ->
            ( {model | lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )

        MouseMove pos ->
            case model.lastMouse of
                Just mousePos ->
                    let delta = {x = toFloat pos.x - mousePos.x, y = toFloat pos.y - mousePos.y}
                    in ( {model | scroll = {x = model.scroll.x + delta.x, y = model.scroll.y + delta.y}, lastMouse = Just {x = toFloat pos.x, y = toFloat pos.y}}, Cmd.none )
                Nothing ->
                    (model, Cmd.none)


view : Model -> Html Msg
view model =
    div [] <|
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
                                        [ Sa.x1 <| toString <| floor <| renderScale * toFloat x + toFloat (round model.scroll.x % renderScale)
                                        , Sa.y1 "0"
                                        , Sa.x2 <| toString <| floor <| renderScale * toFloat x + toFloat (round model.scroll.x % renderScale)
                                        , Sa.y2 <| toString <| pos.y
                                        , Sa.stroke "black"
                                        , Sa.strokeWidth "0.2"
                                        ]
                                        []
                                )
                             <|
                                List.range 0 <|
                                    floor <|
                                        pos.x / renderScale
                            )
                                ++ (List.map
                                        (\y ->
                                            S.line
                                                [ Sa.x1 "0"
                                                , Sa.y1 <| toString <| floor <| renderScale * toFloat y + toFloat (round model.scroll.y % renderScale)
                                                , Sa.x2 <| toString <| pos.x
                                                , Sa.y2 <| toString <| floor <| renderScale * toFloat y + toFloat (round model.scroll.y % renderScale)
                                                , Sa.stroke "black"
                                                , Sa.strokeWidth "0.2"
                                                ]
                                                []
                                        )
                                    <|
                                        List.range 0 <|
                                            floor <|
                                                pos.y / renderScale
                                   )

                        Nothing ->
                            []
                    )

                roads =
                    List.map (\road ->
                            S.line
                                [ Sa.x1 <| toString <| floor <| (road.start.x * renderScale) + model.scroll.x
                                , Sa.y1 <| toString <| floor <| (road.start.y * renderScale) + model.scroll.y
                                , Sa.x2 <| toString <| floor <| (road.end.x * renderScale) + model.scroll.x
                                , Sa.y2 <| toString <| floor <| (road.end.y * renderScale) + model.scroll.y
                                , Sa.strokeWidth <| toString <| renderScale * road.width
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
                                                roadEdge1x = (road.end.x + (Tuple.first <| fromPolar (road.width / 2, roadRot - pi / 2))) * renderScale + model.scroll.x
                                                roadEdge1y = (road.end.y + (Tuple.second <| fromPolar (road.width / 2, roadRot - pi / 2))) * renderScale + model.scroll.y
                                                otherRoadEdge1x = (otherRoad.start.x + (Tuple.first <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * renderScale + model.scroll.x
                                                otherRoadEdge1y = (otherRoad.start.y + (Tuple.second <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * renderScale + model.scroll.y
                                                roadEdge2x = (road.end.x - (Tuple.first <| fromPolar (road.width / 2, roadRot - pi / 2))) * renderScale + model.scroll.x
                                                roadEdge2y = (road.end.y - (Tuple.second <| fromPolar (road.width / 2, roadRot - pi / 2))) * renderScale + model.scroll.y
                                                otherRoadEdge2x = (otherRoad.start.x - (Tuple.first <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * renderScale + model.scroll.x
                                                otherRoadEdge2y = (otherRoad.start.y - (Tuple.second <| fromPolar (otherRoad.width / 2, otherRoadRot - pi / 2))) * renderScale + model.scroll.y
                                            in [ S.polygon
                                                    [ Sa.points <| (toString roadEdge1x) ++ " " ++ (toString roadEdge1y)
                                                         ++ "," ++ (toString otherRoadEdge1x) ++ " " ++ (toString otherRoadEdge1y)
                                                         ++ "," ++ (toString <| road.end.x * renderScale + model.scroll.x) ++ " " ++ (toString <| road.end.y * renderScale + model.scroll.y)
                                                    , Sa.fill "gray"
                                                    ] []
                                                , S.polygon
                                                    [ Sa.points <| (toString roadEdge2x) ++ " " ++ (toString roadEdge2y)
                                                         ++ "," ++ (toString <| road.end.x * renderScale + model.scroll.x) ++ " " ++ (toString <| road.end.y * renderScale + model.scroll.y)
                                                         ++ "," ++ (toString otherRoadEdge2x) ++ " " ++ (toString otherRoadEdge2y)
                                                    , Sa.fill "gray"
                                                    ] []

                                            ]
                                        Nothing ->
                                            []
                                ) road.connectedTo
                        ) model.roads
                roadLines =
                    List.map (\road ->
                            S.line
                                [ Sa.x1 <| toString <| floor <| road.start.x * renderScale + model.scroll.x
                                , Sa.y1 <| toString <| floor <| road.start.y * renderScale + model.scroll.y
                                , Sa.x2 <| toString <| floor <| road.end.x * renderScale + model.scroll.x
                                , Sa.y2 <| toString <| floor <| road.end.y * renderScale + model.scroll.y
                                , Sa.strokeWidth "5"
                                , Sa.stroke "yellow"
                                , Sa.strokeDasharray "5, 15"
                                ]
                                []
                        ) model.roads
                cars =
                    List.map (\car ->
                            S.image
                              [ Sa.x <| toString <| floor <| model.scroll.x + car.pos.x * renderScale - carWidth / 2
                              , Sa.y <| toString <| floor <| model.scroll.y + car.pos.y * renderScale - carHeight / 2
                              , Sa.width <| toString <| carWidth
                              , Sa.height <| toString <| carHeight
                              , Sa.xlinkHref <| "Textures/Cars/" ++ getImg car
                              , Sa.opacity <| toString car.fade
                              , Sa.transform <|
                                  "rotate("
                                      ++ (toString car.rot)
                                      ++ " "
                                      ++ (toString <| car.pos.x * renderScale + model.scroll.x)
                                      ++ " "
                                      ++ (toString <| car.pos.y * renderScale + model.scroll.y)
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
                                    [ Sa.x <| toString <| (road.end.x + light.offset.x - 0.5) * renderScale + model.scroll.x
                                    , Sa.y <| toString <| (road.end.y + light.offset.y - 0.5) * renderScale + model.scroll.y
                                    , Sa.width <| toString renderScale
                                    , Sa.height <| toString renderScale
                                    , Sa.transform <|
                                        "rotate(" ++ (toString roadRotation) ++
                                              " " ++ (toString <| (road.end.x + light.offset.x) * renderScale + model.scroll.x) ++
                                              " " ++ (toString <| (road.end.y + light.offset.y) * renderScale + model.scroll.y) ++
                                              ")"
                                    , Sa.xlinkHref <| getTrafficLightPath light
                                    ] []
                                ]
                            Nothing -> []
                    ) model.roads
             in
                lines ++ roads ++ roadCaps ++ roadLines ++ cars ++ trafficLights
            )
        ] ++ (
            case model.err of
                Just err ->
                    [ p [style [("color", "red")]] [text <| toString err]
                    ]
                Nothing ->
                    []
        )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Time.every (Time.second / 30) UpdateClient
        , Time.every (Time.second / 10) UpdateServer
        , Mouse.downs MousePress
        , Mouse.ups MouseRelease
        , Mouse.moves MouseMove
        ]

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


renderScale =
    40


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Position =
    { x : Float, y : Float }


pAdd : Position -> Position -> Position
pAdd p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


carHeight : Float
carHeight =
    25


carWidth : Float
carWidth =
    carHeight * 2


type alias Car =
    { name : String
    , pos : Position
    , rot : Float
    , vel : Float -- How many pixels forwards the car should move every second in the direction it's facing
    , steering : Float
    }


type alias Road =
    { start : Position
    , end : Position
    , startRoadIdx : Int
    , endRoadIdx : Int
    }


type alias Traffic =
    { cars : List Car
    , roads : List Road
    }


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
            |> P.required "pos" decodePosition
            |> P.required "rot" Decode.float
            |> P.required "vel" Decode.float
            |> P.required "steering" Decode.float
        )


decodeRoads : Decoder (List Road)
decodeRoads =
    Decode.list
        (P.decode Road
            |> P.required "start" decodePosition
            |> P.required "end" decodePosition
            |> P.required "startRoadIdx" Decode.int
            |> P.required "endRoadIdx" Decode.int
        )


decodePosition : Decoder Position
decodePosition =
    Decode.map2 (\x y -> { x = x, y = y }) (Decode.field "x" Decode.float) (Decode.field "y" Decode.float)


type alias Model =
    { cars : List Car
    , roads : List Road
    , err : Maybe String
    , size : Maybe Position
    , lasttime : Maybe Time
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] [] Nothing Nothing Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTrafficState (Ok traffic) ->
            ( { model | cars = traffic.cars, roads = traffic.roads, err = Nothing }, Cmd.none )

        SetTrafficState (Err error) ->
            ( { model | err = Just <| toString error }, Cmd.none )

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
                            Time.inSeconds (lasttime - time)
                    in
                        ( { model
                            | lasttime = Just time
                            , cars =
                                List.map (\car ->
                                        { car
                                            | pos =
                                                pAdd car.pos <| (\( x, y ) -> { x = negate x, y = negate y }) <| fromPolar ( car.vel * delta, degrees car.rot )
                                            , rot = car.rot + car.steering * delta
                                        }
                                    )
                                    model.cars
                          }
                        , Cmd.none
                        )

        UpdateServer time ->
            ( model, Task.perform identity <| Task.succeed Request )


view : Model -> Html Msg
view model =
    div [] <|
        [ -- Generate svg!
          S.svg
            [ Sa.width <| Maybe.withDefault "10" <| Maybe.map (toString << .x) model.size
            , Sa.height <| Maybe.withDefault "10" <| Maybe.map (toString << .y) model.size
            , Sa.style "background: green"
            ]
            (let
                lines =
                    (case model.size of
                        Just pos ->
                            (List.map (\x ->
                                    S.line
                                        [ Sa.x1 <| toString <| floor <| renderScale * toFloat x
                                        , Sa.y1 "0"
                                        , Sa.x2 <| toString <| floor <| renderScale * toFloat x
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
                                                , Sa.y1 <| toString <| floor <| renderScale * toFloat y
                                                , Sa.x2 <| toString <| pos.x
                                                , Sa.y2 <| toString <| floor <| renderScale * toFloat y
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
                                [ Sa.x1 <| toString <| floor <| (road.start.x * renderScale)
                                , Sa.y1 <| toString <| floor <| (road.start.y * renderScale)
                                , Sa.x2 <| toString <| floor <| (road.end.x * renderScale)
                                , Sa.y2 <| toString <| floor <| (road.end.y * renderScale)
                                , Sa.strokeWidth "40"
                                , Sa.stroke "gray"
                                ]
                                []
                        )
                        model.roads

                cars =
                    List.concatMap (\car ->
                            [ S.image
                                [ Sa.x <| toString <| floor <| (car.pos.x * renderScale) - carWidth / 2
                                , Sa.y <| toString <| floor <| (car.pos.y * renderScale) - carHeight / 2
                                , Sa.width <| toString <| carWidth
                                , Sa.height <| toString <| carHeight
                                , Sa.xlinkHref "/Cars/Car1.png"
                                , Sa.transform <|
                                    "rotate("
                                        ++ (toString car.rot)
                                        ++ " "
                                        ++ (toString <| car.pos.x * renderScale)
                                        ++ " "
                                        ++ (toString <| car.pos.y * renderScale)
                                        ++ ")"
                                ]
                                []
                            , S.text_
                                [ Sa.x <| toString <| car.pos.x * renderScale
                                , Sa.y <| toString <| (car.pos.y + 0.1) * renderScale
                                , Sa.textAnchor "middle"
                                , Sa.fill "blue"
                                , Sa.fontWeight "bold"
                                ]
                                [ S.text car.name ]
                            ]
                        )
                        model.cars
             in
                lines ++ roads ++ cars
            )
        ]
            ++ [ button [ onClick Request ] [ text "Request!" ]
               ]
            ++ (case model.err of
                    Just err ->
                        [ p [ style [ ( "color", "red" ) ] ] [ text <| "Error: " ++ toString model.err ] ]

                    Nothing ->
                        []
               )
            ++ (case model.size of
                    Just size ->
                        [ p [] [ text <| "Size: " ++ toString model.size ] ]

                    Nothing ->
                        []
               )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Time.every (Time.second / 30) UpdateClient
        , Time.every (Time.second / 10) UpdateServer
        ]

module JsonParser exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as P
import Json.Encode as Enc

import Base exposing (..)

decodeTraffic : Decoder Traffic
decodeTraffic =
    P.decode Traffic
        |> P.required "cars" decodeCars
        |> P.required "roads" decodeRoads
        |> P.custom (Decode.at [ "you", "ip" ] Decode.string)
        |> P.custom (Decode.at [ "you", "info", "perms" ] ( Decode.list Decode.string ))
        |> P.custom (Decode.at [ "you", "info", "loggedIn" ] Decode.bool)
        |> P.required "others" decodeOthers

decodeOthers =
    Decode.list
        (P.decode User
            |> P.required "ip" Decode.string
            |> P.required "name" Decode.string)


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
            |> P.optional "police" Decode.bool False
            |> P.custom (Decode.maybe (Decode.field "controlled_by" Decode.string))
        )


decodeRoads : Decoder (List Road)
decodeRoads =
    Decode.list
        (P.decode Road
            |> P.required "id" Decode.string
            |> P.required "start" decodePosition
            |> P.required "end" decodePosition
            |> P.required "connected_to" (Decode.list Decode.string)
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

encodeProtoCar : ProtoCar -> Enc.Value
encodeProtoCar car =
    Enc.object
        [ ("img", Enc.string car.img)
        , ("pos", encodePos car.pos)
        , ("is_police", Enc.bool car.isPolice)
        ]

encodePos pos =
    Enc.object
        [ ("x", Enc.float pos.x)
        , ("y", Enc.float pos.y)
        ]

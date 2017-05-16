module TrafficRenderer exposing (renderRoads, renderCars, renderTrafficLights, renderBackgroundLines)

import Base exposing (..)
import Svg as S
import Svg.Attributes as Sa

renderRoads : Model -> List Road -> List (S.Svg Msg)
renderRoads model roads =
    (
        List.map (\road ->
            S.line (
            [ Sa.x1 <| toString <| (road.start.x * model.renderScale) + model.scroll.x
            , Sa.y1 <| toString <| (road.start.y * model.renderScale) + model.scroll.y
            , Sa.x2 <| toString <| (road.end.x * model.renderScale) + model.scroll.x
            , Sa.y2 <| toString <| (road.end.y * model.renderScale) + model.scroll.y
            , Sa.strokeWidth <| toString <| model.renderScale * road.width + 2
            , Sa.stroke "gray"
            ] ++ (if List.member road.id model.hiddenRoads then [Sa.opacity "0.5"] else [])
            )
            []
        )
        roads
    ) ++ renderRoadsCaps model roads
      ++ renderRoadLines model roads

renderRoadsCaps : Model -> List Road -> List (S.Svg Msg)
renderRoadsCaps model roads =
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
                    ) <| List.filterMap (\x -> id2idx model x) road.connectedTo
            ) roads

renderRoadLines : Model -> List Road -> List (S.Svg Msg)
renderRoadLines model roads =
    List.map (\road ->
            S.line
                [ Sa.x1 <| toString <| road.start.x * model.renderScale + model.scroll.x
                , Sa.y1 <| toString <| road.start.y * model.renderScale + model.scroll.y
                , Sa.x2 <| toString <| road.end.x * model.renderScale + model.scroll.x
                , Sa.y2 <| toString <| road.end.y * model.renderScale + model.scroll.y
                , Sa.strokeWidth <| toString <| model.renderScale / 6
                , Sa.stroke "yellow"
                , Sa.strokeDasharray <| (toString <| model.renderScale / 6) ++ ", " ++ (toString <| model.renderScale / 3)
                ]
                []
        ) roads


renderCars : Model -> List Car -> List (S.Svg Msg)
renderCars model cars =
    List.concatMap (\car ->
            let size = model.renderScale * car.size
            in
                [ S.image
                  [ Sa.x <| toString <| model.scroll.x + car.pos.x * model.renderScale - carWidth / 2 * size
                  , Sa.y <| toString <| model.scroll.y + car.pos.y * model.renderScale - carHeight / 2 * size
                  , Sa.width <| toString <| carWidth * size
                  , Sa.height <| toString <| carHeight * size
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
                ] ++ (
                case car.controlledBy of
                    Just ip ->
                        let users = List.filter (\user -> user.ip == ip) model.others
                            user = List.head users
                        in case user of
                            Just user ->
                                [ S.text_
                                  [ Sa.x <| toString <| model.scroll.x + car.pos.x * size
                                  , Sa.y <| toString <| model.scroll.y + (car.pos.y + 1) * size
                                  , Sa.fill "white"
                                  , Sa.fontFamily "Arial"
                                  , Sa.textAnchor "middle"
                                  , Sa.fontSize <| toString <| size / 2
                                  ]
                                    [ S.text user.name ]
                                ]
                            Nothing -> []
                    Nothing -> []
                )

        )
        cars


renderTrafficLights : Model -> List Road -> List (S.Svg Msg)
renderTrafficLights model roads =
    List.filterMap (\road ->
        case road.trafficLight of
            Just light ->
                let roadDelta = {x = road.end.x - road.start.x, y = road.end.y - road.start.y}
                    roadRotation = Tuple.second <| toPolar (roadDelta.x, roadDelta.y)
                    roadRotationDeg = roadRotation / pi * 180 + 90
                    roadLength = Tuple.first <| toPolar (roadDelta.x, roadDelta.y)
                    roadDelta_ = fromPolar (roadLength - light.at, roadRotation)
                    roadEnd_ = {x = road.start.x + Tuple.first roadDelta_, y = road.start.y + Tuple.second roadDelta_}
                    (dx, dy) = fromPolar (light.offset, roadRotation + pi / 2)
                in Just <|
                   S.image
                    [ Sa.x <| toString <| (roadEnd_.x + dx - 0.5) * model.renderScale + model.scroll.x
                    , Sa.y <| toString <| (roadEnd_.y + dy - 0.5) * model.renderScale + model.scroll.y
                    , Sa.width <| toString model.renderScale
                    , Sa.height <| toString model.renderScale
                    , Sa.transform <|
                        "rotate(" ++ (toString roadRotationDeg) ++
                              " " ++ (toString <| (roadEnd_.x + dx) * model.renderScale + model.scroll.x) ++
                              " " ++ (toString <| (roadEnd_.y + dy) * model.renderScale + model.scroll.y) ++
                              ")"
                    , Sa.xlinkHref <| getTrafficLightPath light
                    ] []
            Nothing -> Nothing
    ) roads


renderBackgroundLines : Model -> List (S.Svg Msg)
renderBackgroundLines model =
    case model.size of
        Just pos ->
            (List.map (\x ->
                    S.line
                        [ Sa.x1 <| toString <| model.renderScale * toFloat x + toFloat (round model.scroll.x % round model.renderScale)
                        , Sa.y1 "0"
                        , Sa.x2 <| toString <| model.renderScale * toFloat x + toFloat (round model.scroll.x % round model.renderScale)
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
                                , Sa.y1 <| toString <| model.renderScale * toFloat y + toFloat (round model.scroll.y % round model.renderScale)
                                , Sa.x2 <| toString <| pos.x
                                , Sa.y2 <| toString <| model.renderScale * toFloat y + toFloat (round model.scroll.y % round model.renderScale)
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

module MenuRenderer exposing (..)

import Svg as S
import Svg.Attributes as Sa
import Svg.Events as Sev

import Base exposing (..)

renderMenu : Model -> List (S.Svg Msg)
renderMenu model =
    case model.size of
        Just size ->
            let ballPosition = {x = size.x - model.menu.radius * 1.5, y = size.y - model.menu.radius * 1.5}
                ballRot = model.menu.rotation * pi / 4
                width = model.menu.radius + model.menu.rotation * model.menu.height * (toFloat <| List.length model.menu.buttons)
                currentMenuWidth = model.menu.rotation * width
            in 
                [ S.rect
                    [ Sa.x <| toString <| ballPosition.x - currentMenuWidth
                    , Sa.y <| toString <| ballPosition.y - model.menu.height / 2
                    , Sa.width <| toString <| currentMenuWidth
                    , Sa.height <| toString <| model.menu.height
                    , Sa.style "fill:white"
                    , Sa.fillOpacity "0.5"
                    ] []
                , S.circle
                    [ Sa.cx <| toString <| floor ballPosition.x
                    , Sa.cy <| toString <| floor ballPosition.y
                    , Sa.r  <| toString model.menu.radius
                    , Sa.style "fill:white"
                    , Sev.onClick MenuBallClicked
                    ] []
                , S.line
                    [ Sa.x1 <| toString <| ballPosition.x - model.menu.radius * (cos <| negate ballRot)
                    , Sa.y1 <| toString <| ballPosition.y - model.menu.radius * (sin <| negate ballRot)
                    , Sa.x2 <| toString <| ballPosition.x + model.menu.radius * (cos <| negate ballRot)
                    , Sa.y2 <| toString <| ballPosition.y + model.menu.radius * (sin <| negate ballRot)
                    , Sev.onClick MenuBallClicked
                    , Sa.strokeWidth "2"
                    , Sa.stroke "gray"
                    ] []
                , S.line
                    [ Sa.x1 <| toString <| ballPosition.x - model.menu.radius * sin ballRot
                    , Sa.y1 <| toString <| ballPosition.y - model.menu.radius * cos ballRot
                    , Sa.x2 <| toString <| ballPosition.x + model.menu.radius * sin ballRot
                    , Sa.y2 <| toString <| ballPosition.y + model.menu.radius * cos ballRot
                    , Sev.onClick MenuBallClicked
                    , Sa.strokeWidth "2"
                    , Sa.stroke "gray"
                    ] []
                , S.clipPath
                    [ Sa.id "rectPath"]
                    [ S.rect
                        [ Sa.x <| toString <| ballPosition.x - currentMenuWidth
                        , Sa.y <| toString <| ballPosition.y - model.menu.height / 2
                        , Sa.width <| toString <| currentMenuWidth
                        , Sa.height <| toString <| model.menu.height
                        , Sa.style "fill:white"
                        , Sa.fillOpacity "0.5"
                        ] []
                    ]
                ] ++ ( List.concat <| List.indexedMap (\i button -> 
                    [ S.image
                        [ Sa.xlinkHref <| "Textures/Buttons/" ++ button.image ++ ".png"
                        , Sa.x <| toString <| ballPosition.x - currentMenuWidth + model.menu.height * toFloat i
                        , Sa.y <| toString <| ballPosition.y - model.menu.height / 2
                        , Sa.height <| toString <| model.menu.height
                        , Sa.clipPath "url(#rectPath)"
                        ] []
                    , S.rect
                        [ Sa.x <| toString <| ballPosition.x - currentMenuWidth + model.menu.height * toFloat i
                        , Sa.y <| toString <| ballPosition.y - model.menu.height / 2
                        , Sa.width <| toString <| model.menu.height
                        , Sa.height <| toString <| model.menu.height
                        , Sa.clipPath "url(#rectPath)"
                        , Sa.opacity "0"
                        , Sev.onClick <| MenuButtonClicked button.message
                        ] []
                    ]
                    ) model.menu.buttons)
        Nothing -> []

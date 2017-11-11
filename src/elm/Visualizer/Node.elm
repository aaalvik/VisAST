module Visualizer.Node exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


drawNode : String -> Int -> Int -> Svg msg
drawNode name x y =
    svg [ class "nodes" ]
        [ drawCircle x y
        , drawText name x y
        ]


drawCircle : Int -> Int -> Svg msg
drawCircle xPos yPos =
    circle [ cx (toString xPos), cy (toString yPos), r "50", fill "#F0AD00" ] []


drawText : String -> Int -> Int -> Svg msg
drawText name xPos yPos =
    text_ [ alignmentBaseline "middle", textAnchor "middle", x (toString xPos), y (toString yPos) ] [ text name ]

module Visualizer.Node exposing (..)

import SimpleAST exposing (Expr(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)


wFACTOR =
    11


startX =
    100


startY =
    100


drawNode : String -> Int -> Int -> List (Svg msg)
drawNode name x y =
    let
        wRect =
            nodeWidth name + 20

        hRect =
            30
    in
    [ drawRectangle x y wRect hRect
    , drawText name (x + wRect // 2) (y + (hRect // 2))
    ]


drawRectangle : Int -> Int -> Int -> Int -> Svg msg
drawRectangle xPos yPos w h =
    rect [ height (toString h), width (toString w), x (toString xPos), y (toString yPos), rx "15", ry "15", fill "pink" ] []


drawCircle : Int -> Int -> Svg msg
drawCircle xPos yPos =
    circle [ cx (toString xPos), cy (toString yPos), r "50", fill "#F0AD00" ] []


drawText : String -> Int -> Int -> Svg msg
drawText name xPos yPos =
    text_ [ alignmentBaseline "middle", textAnchor "middle", x (toString xPos), y (toString yPos) ] [ text name ]


drawTree : Expr -> Svg msg
drawTree tree =
    drawNode "Num" startX startY
        ++ drawNode "4" (startX - 25) (startY + 50)
        ++ drawNode "3" (startX + 50) (startY + 50)
        |> svg [ class "nodes" ]



-- WIDTH HELPERS


treeWidth : Expr -> Int
treeWidth expr =
    case expr of
        Num n ->
            nodeWidth (toString n)

        Var str ->
            nodeWidth str

        Neg expr ->
            treeWidth expr

        Add expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2

        Mul expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2

        Sub expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2

        LessThan expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2

        If boolExpr expr1 expr2 ->
            treeWidth boolExpr + treeWidth expr1 + treeWidth expr2

        SetVar var body ->
            nodeWidth var + treeWidth body

        SetFun fName argNamesList body ->
            nodeWidth fName
                + List.foldl (\name acc -> acc + nodeWidth name) 0 argNamesList
                + treeWidth body

        Fun argNamesList body env ->
            List.foldl (\name acc -> acc + nodeWidth name) 0 argNamesList
                + treeWidth body
                + nodeWidth (toString env)

        Apply fName argList ->
            nodeWidth fName
                + List.foldl (\arg acc -> acc + treeWidth arg) 0 argList

        Seq exprList ->
            List.foldl (\expr acc -> acc + treeWidth expr) 0 exprList

        Error str ->
            nodeWidth str


nodeWidth : String -> Int
nodeWidth name =
    String.length name * wFACTOR

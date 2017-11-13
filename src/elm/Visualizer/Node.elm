module Visualizer.Node exposing (..)

import SimpleAST exposing (Expr(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)


drawNode : Int -> Int -> String -> List (Svg msg)
drawNode x y name =
    let
        wRect =
            nodeWidth name + 10

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
    drawSubTree startX startY tree
        |> svg [ class "nodes" ]


drawSubTree : Int -> Int -> Expr -> List (Svg msg)
drawSubTree x y tree =
    let
        mid name =
            x + (nodeWidth name // 2)

        totalWidth =
            treeWidth tree

        newY =
            nextY y

        xLeft name numChildren =
            mid name - (treeWidth tree // numChildren)
    in
    case tree of
        (Num num) as n ->
            drawNode x y (toString n)

        (Var str) as v ->
            drawNode x y (toString v)

        Neg expr ->
            drawNode x y "Neg"
                ++ drawSubTree x newY expr

        Add expr1 expr2 ->
            drawNode x y "Add"
                ++ drawSubTree (xLeft "Add" 2) newY expr1
                ++ drawSubTree (xLeft "Add" 2 + treeWidth expr1 + 10) newY expr2

        Mul expr1 expr2 ->
            drawNode x y "Mul"
                ++ drawSubTree (xLeft "Mul" 2) newY expr1
                ++ drawSubTree (xLeft "Mul" 2 + treeWidth expr1 + 10) newY expr2

        Sub expr1 expr2 ->
            drawNode x y "Sub"
                ++ drawSubTree (xLeft "Sub" 2) newY expr1
                ++ drawSubTree (xLeft "Sub" 2 + treeWidth expr1 + 10) newY expr2

        LessThan expr1 expr2 ->
            drawNode x y "LessThan"
                ++ drawSubTree (xLeft "LessThan" 2) newY expr1
                ++ drawSubTree (xLeft "LessThan" 2 + treeWidth expr1 + 10) newY expr2

        If boolExpr expr1 expr2 ->
            drawNode x y "TODO"

        SetVar var body ->
            drawNode x y "TODO"

        SetFun fName argNamesList body ->
            drawNode x y "TODO"

        Fun argNamesList body env ->
            drawNode x y "TODO"

        Apply fName argList ->
            drawNode x y "TODO"

        Seq exprList ->
            drawNode x y "TODO"

        Error str ->
            drawNode x y "TODO"



-- drawNode "Num" startX startY
--     ++ drawNode "4" (startX - 25) (nextY startY)
--     ++ drawNode "30" (startX + 50) (nextY startY)
--     |> svg [ class "nodes" ]
-- WIDTH HELPERS


treeWidth : Expr -> Int
treeWidth expr =
    case expr of
        (Num num) as n ->
            nodeWidth (toString n) + 10

        (Var str) as s ->
            nodeWidth (toString s) + 10

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


wFACTOR =
    9


startX =
    100


startY =
    100


nextY y =
    y + marginY


marginY =
    45


marginX =
    30

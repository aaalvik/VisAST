module Visualizer.Node exposing (..)

import SimpleAST exposing (Expr(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)


drawNode : Int -> Int -> String -> List (Svg msg) -> List (Svg msg)
drawNode xMid y name children =
    let
        wRect =
            nodeWidth name + 10

        hRect =
            30

        xAnchor =
            xMid - wRect // 2
    in
    [ drawRectangle xAnchor y wRect hRect
    , drawText name xMid (y + (hRect // 2))
    ]
        ++ children


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
drawSubTree xMid y tree =
    let
        totalWidth =
            treeWidth tree

        newY =
            nextY y
    in
    case tree of
        (Num num) as n ->
            drawNode xMid y (toString n) []

        (Var str) as v ->
            drawNode xMid y (toString v) []

        Neg expr ->
            drawNode xMid y "Neg" (drawSubTree xMid newY expr)

        Add expr1 expr2 ->
            let
                children =
                    makeChildren "Add" xMid y newY totalWidth [ expr1, expr2 ]
            in
            drawNode xMid y "Add" children

        --drawBinOpNode "Add" expr1 expr2 xMid y newY
        -- Mul expr1 expr2 ->
        --     drawBinOpNode "Mul" expr1 expr2 xMid y newY
        -- LessThan expr1 expr2 ->
        --     drawBinOpNode "LessThan" expr1 expr2 xMid y newY
        -- Sub expr1 expr2 ->
        --     drawBinOpNode "Sub" expr1 expr2 xMid y newY
        --If b e1 e2 ->
        --     let
        --         xLeft =
        --             xFirst "If"
        --     in
        --     drawNode x y "If" []
        --         ++ drawSubTree xLeft newY b
        --         ++ drawSubTree (xLeft + treeWidth b + 10) newY e1
        --         ++ drawSubTree (xLeft + treeWidth b + treeWidth e1 + 20) newY e2
        _ ->
            drawNode xMid y "TODO" []



-- SetVar var body ->
--     drawNode x y "TODO" []
-- SetFun fName argNamesList body ->
--     drawNode x y "TODO" []
-- Fun argNamesList body env ->
--     drawNode x y "TODO" []
-- Apply fName argList ->
--     drawNode x y "TODO" []
-- Seq exprList ->
--     drawNode x y "TODO" []
-- Error str ->
--     drawNode x y "TODO" []
-- drawNode "Num" startX startY
--     ++ drawNode "4" (startX - 25) (nextY startY)
--     ++ drawNode "30" (startX + 50) (nextY startY)
--     |> svg [ class "nodes" ]
-- WIDTH HELPERS


makeChildren : String -> Int -> Int -> Int -> Int -> List Expr -> List (Svg msg)
makeChildren name x y childY w childrenExprs =
    let
        leftX =
            x - w // 2

        childrenXs =
            List.foldl
                (\child ( nextX, xs ) ->
                    ( nextX + treeWidth child // 2
                    , if List.isEmpty xs then
                        [ nextX ]
                      else
                        xs ++ [ nextX + treeWidth child ]
                    )
                )
                ( leftX, [] )
                childrenExprs
                |> Tuple.second

        children =
            List.concat <| List.map2 (\child childX -> drawSubTree childX childY child) childrenExprs childrenXs
    in
    drawNode x y name children



-- drawBinOpNode name e1 e2 x y newY =
--     let
--         xLeft =
--             x - treeWidth e1
--         xRight =
--             x + treeWidth e2
--         children =
--             drawSubTree xLeft newY e1
--                 ++ drawSubTree xRight newY e2
--     in
--     drawNode x y name children


maxWidth : List Expr -> Int
maxWidth exprs =
    List.foldl (\expr acc -> Basics.max (treeWidth expr) acc) 0 exprs


treeWidth : Expr -> Int
treeWidth expr =
    case expr of
        (Num num) as n ->
            nodeWidth (toString n)

        (Var str) as s ->
            nodeWidth (toString s)

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
            treeWidth boolExpr + treeWidth expr1 + treeWidth expr2 + 2

        SetVar var body ->
            nodeWidth var + treeWidth body

        SetFun fName argNamesList body ->
            nodeWidth fName
                + List.foldl (\name acc -> acc + nodeWidth name) 0 argNamesList
                + treeWidth body

        -- + (List.length argNamesList + 2)
        -- * marginX
        Fun argNamesList body env ->
            List.foldl (\name acc -> acc + nodeWidth name) 0 argNamesList
                + treeWidth body
                + nodeWidth (toString env)

        -- + (List.length argNamesList + 2)
        -- * marginX
        Apply fName argList ->
            nodeWidth fName
                + List.foldl (\arg acc -> acc + treeWidth arg) 0 argList

        -- + (List.length argList + 1)
        -- * marginX
        Seq exprList ->
            List.foldl (\expr acc -> acc + treeWidth expr) 0 exprList

        -- + List.length exprList
        -- * marginX
        Error str ->
            nodeWidth str


nodeWidth : String -> Int
nodeWidth name =
    String.length name * wFACTOR + marginBetween


marginBetween : Int
marginBetween =
    10


wFACTOR : Int
wFACTOR =
    9


startX : Int
startX =
    300


startY : Int
startY =
    100


nextY : Int -> Int
nextY y =
    y + marginY


marginY : Int
marginY =
    45



-- marginX =
--     10

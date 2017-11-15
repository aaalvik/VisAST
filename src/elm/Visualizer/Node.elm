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
            maxTreeWidth tree

        --treeWidth tree
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

        Mul expr1 expr2 ->
            let
                children =
                    makeChildren "Mul" xMid y newY totalWidth [ expr1, expr2 ]
            in
            drawNode xMid y "Mul" children

        Sub expr1 expr2 ->
            let
                children =
                    makeChildren "Sub" xMid y newY totalWidth [ expr1, expr2 ]
            in
            drawNode xMid y "Sub" children

        If bool eThen eElse ->
            let
                children =
                    makeChildren "If" xMid y newY totalWidth [ bool, eThen, eElse ]
            in
            drawNode xMid y ("If: " ++ toString (totalWidth // 3)) children

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



-- WIDTH HELPERS


makeChildren : String -> Int -> Int -> Int -> Int -> List Expr -> List (Svg msg)
makeChildren name x y childY w childrenExprs =
    let
        leftX =
            x - w // 2

        numChildren =
            List.length childrenExprs

        diff =
            w // (numChildren - 1)

        xs =
            Tuple.second <| List.foldl (\_ ( x, acc ) -> ( x + diff, acc ++ [ x + diff ] )) ( leftX, [ leftX ] ) (List.range 2 numChildren)

        -- List.foldl
        --     (\child ( nextX, xs ) ->
        --         ( nextX + treeWidth child // 2 + 10
        --         , if List.isEmpty xs then
        --             [ nextX ]
        --           else
        --             xs ++ [ nextX + treeWidth child ]
        --         )
        --     )
        --     ( leftX + 10, [] )
        --     childrenExprs
        --     |> Tuple.second
        children =
            List.concat <| List.map2 (\child childX -> drawSubTree childX childY child) childrenExprs xs
    in
    children


maximum : (a -> Int) -> List a -> Int
maximum widthFunction list =
    List.foldl (\element acc -> Basics.max (widthFunction element) acc) 0 list


maxTreeWidth : Expr -> Int
maxTreeWidth expr =
    case expr of
        (Num num) as n ->
            nodeWidth (toString n)

        (Var str) as s ->
            nodeWidth (toString s)

        Neg expr ->
            maxTreeWidth expr

        Add expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        Mul expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        Sub expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        LessThan expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        If boolExpr expr1 expr2 ->
            3 * maximum maxTreeWidth [ boolExpr, expr1, expr2 ]

        SetVar var body ->
            2 * Basics.max (nodeWidth var) (maxTreeWidth body)

        SetFun fName argNamesList body ->
            3 * Basics.max (maximum nodeWidth <| fName :: argNamesList) (maxTreeWidth body)

        Fun argNamesList body env ->
            3 * Basics.max (maxTreeWidth body) (maximum nodeWidth <| toString env :: argNamesList)

        Apply fName argList ->
            2 * Basics.max (nodeWidth fName) (maximum maxTreeWidth argList)

        Seq exprList ->
            List.length exprList * maximum maxTreeWidth exprList

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

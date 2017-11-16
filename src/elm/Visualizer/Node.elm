module Visualizer.Node exposing (..)

import SimpleAST exposing (Expr(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualizer.Helpers.Width exposing (maxTreeWidth, nodeWidth)


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


drawText : String -> Int -> Int -> Svg msg
drawText name xPos yPos =
    text_ [ alignmentBaseline "middle", textAnchor "middle", x (toString xPos), y (toString yPos) ] [ text name ]


drawTree : Maybe Expr -> Svg msg
drawTree mTree =
    case mTree of
        Nothing ->
            drawText "..." 100 50

        Just tree ->
            let
                w =
                    maxTreeWidth tree

                startX =
                    w // 2

                startY =
                    50

                viewBoxValues =
                    "0 0 " ++ toString (w + 50) ++ " 300"
            in
            drawSubTree startX startY tree
                |> svg [ class "tree", viewBox viewBoxValues ]


drawSubTree : Int -> Int -> Expr -> List (Svg msg)
drawSubTree xMid y tree =
    let
        totalWidth =
            maxTreeWidth tree

        newY =
            nextY y

        leftX =
            xMid - totalWidth // 2

        rightX =
            leftX + totalWidth
    in
    case tree of
        Num num ->
            drawNode xMid y "Num" <| drawNode xMid newY (toString num) []

        Var str ->
            drawNode xMid y "Var" <| drawNode xMid newY (toString str) []

        Neg expr ->
            drawNode xMid y "Neg" <| drawSubTree xMid newY expr

        Add expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Add" [ expr1, expr2 ]

        Mul expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Mul" [ expr1, expr2 ]

        Sub expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Sub" [ expr1, expr2 ]

        LessThan expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "LessThan" [ expr1, expr2 ]

        If bool eThen eElse ->
            let
                children =
                    makeChildren xMid newY totalWidth drawSubTree [ bool, eThen, eElse ]
            in
            drawNode xMid y "If" children

        SetVar varName body ->
            let
                children =
                    drawNode leftX newY varName []
                        ++ drawSubTree rightX newY body
            in
            drawNode xMid y "SetVar" children

        SetFun fName argNames body ->
            let
                wArgs =
                    List.length argNames * List.foldl (\arg acc -> Basics.max acc <| nodeWidth arg) 0 argNames

                args =
                    drawNode xMid newY "ArgNames" <| makeChildren xMid (nextY newY) wArgs (\x y str -> drawNode x y (toString str) []) argNames

                children =
                    drawNode leftX newY fName []
                        ++ args
                        ++ drawSubTree rightX newY body
            in
            drawNode xMid y "SetFun" children

        Fun argNames body env ->
            let
                wArgs =
                    List.length argNames * List.foldl (\arg acc -> Basics.max acc <| nodeWidth arg) 0 argNames

                args =
                    drawNode leftX newY "ArgNames" <| makeChildren leftX (nextY newY) wArgs (\x y str -> drawNode x y (toString str) []) argNames

                children =
                    args
                        ++ drawSubTree xMid newY body
                        ++ drawNode rightX newY (toString env) []
            in
            drawNode xMid y "Fun" children

        Apply fName args ->
            let
                wArgs =
                    List.length args * List.foldl (\arg acc -> Basics.max acc <| maxTreeWidth arg) 0 args

                argsTree =
                    drawNode rightX newY "Args" <| makeChildren rightX (nextY newY) wArgs drawSubTree args

                children =
                    drawNode leftX newY (toString fName) []
                        ++ argsTree
            in
            drawNode xMid y "Apply" children

        Seq exprList ->
            let
                wChildren =
                    List.length exprList * List.foldl (\expr acc -> Basics.max acc <| maxTreeWidth expr) 0 exprList

                children =
                    makeChildren xMid newY wChildren drawSubTree exprList
            in
            drawNode xMid y "Seq" children

        Error str ->
            drawNode xMid y ("ERROR: " ++ str) []


drawBinOp : Int -> Int -> Int -> Int -> String -> List Expr -> List (Svg msg)
drawBinOp x y newY w name childrenList =
    let
        children =
            makeChildren x newY w drawSubTree childrenList
    in
    drawNode x y name children


makeChildren : Int -> Int -> Int -> (Int -> Int -> a -> List (Svg msg)) -> List a -> List (Svg msg)
makeChildren x y w drawFunction childrenExprs =
    let
        leftX =
            x - w // 2

        numChildren =
            List.length childrenExprs

        diff =
            w // (numChildren - 1)

        xs =
            Tuple.second <| List.foldl (\_ ( x, acc ) -> ( x + diff, acc ++ [ x + diff ] )) ( leftX, [ leftX ] ) (List.range 2 numChildren)

        children =
            List.concat <| List.map2 (\child curX -> drawFunction curX y child) childrenExprs xs
    in
    children


nextY : Int -> Int
nextY y =
    y + marginY


marginY : Int
marginY =
    45

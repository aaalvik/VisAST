module Visualizer.Node exposing (..)

import SimpleAST exposing (Expr(..))
import Svg exposing (..)
import Svg.Attributes as Attrs exposing (..)
import Visualizer.Helpers.Width exposing (maxTreeWidth, nodeWidth)


type alias EdgePosition =
    ( ( Int, Int ), ( Int, Int ) )


type alias Position =
    ( Int, Int )


drawNode : Int -> Int -> String -> List (Svg msg) -> List (Svg msg)
drawNode x y name children =
    let
        wRect =
            nodeWidth name + 10

        hRect =
            30

        childY =
            nextY y

        xAnchor =
            x - wRect // 2
    in
    [ drawRectangle xAnchor y wRect hRect
    , drawText name x (y + (hRect // 2))
    ]
        ++ children


drawRectangle : Int -> Int -> Int -> Int -> Svg msg
drawRectangle xPos yPos w h =
    rect [ height (toString h), width (toString w), x (toString xPos), y (toString yPos), rx "15", ry "15", fill "#128277" ] []


drawText : String -> Int -> Int -> Svg msg
drawText name xPos yPos =
    text_ [ alignmentBaseline "middle", textAnchor "middle", x (toString xPos), y (toString yPos), fill "white" ] [ text name ]


drawEdge : EdgePosition -> Svg msg
drawEdge ( ( parentX, parentY ), ( childX, childY ) ) =
    line
        [ x1 (toString parentX)
        , y1 (toString <| parentY + 15)
        , x2 (toString childX)
        , y2 (toString <| childY + 15)
        , Attrs.style "stroke:#004D47;stroke-width:1"
        ]
        []


drawTree : Maybe Expr -> Svg msg
drawTree mTree =
    case mTree of
        Nothing ->
            drawText "..." 100 50

        Just tree ->
            let
                startX =
                    500

                startY =
                    50
            in
            drawSubTree startX startY tree
                |> svg [ class "tree" ]


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
            let
                edge =
                    drawEdge ( ( xMid, y ), ( xMid, newY ) )
            in
            edge :: (drawNode xMid y "Num" <| drawNode xMid newY (toString num) [])

        Var str ->
            let
                edge =
                    drawEdge ( ( xMid, y ), ( xMid, newY ) )
            in
            edge :: (drawNode xMid y "Var" <| drawNode xMid newY (toString str) [])

        Neg expr ->
            let
                edge =
                    drawEdge ( ( xMid, y ), ( xMid, newY ) )
            in
            edge :: (drawNode xMid y "Neg" <| drawSubTree xMid newY expr)

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
                edges =
                    drawEdges xMid y totalWidth 3

                children =
                    makeChildren xMid newY totalWidth drawSubTree [ bool, eThen, eElse ]
            in
            edges ++ drawNode xMid y "If" children

        SetVar varName body ->
            let
                edges =
                    drawEdges xMid y totalWidth 2

                children =
                    drawNode leftX newY (toString varName) []
                        ++ drawSubTree rightX newY body
            in
            edges ++ drawNode xMid y "SetVar" children

        SetFun fName argNames body ->
            let
                edges =
                    drawEdges xMid y totalWidth 3

                wArgs =
                    List.length argNames * List.foldl (\arg acc -> Basics.max acc <| nodeWidth arg) 0 argNames

                argsEdges =
                    drawEdges xMid newY wArgs (List.length argNames)

                args =
                    argsEdges
                        ++ (drawNode xMid newY "ArgNames" <| makeChildren xMid (nextY newY) wArgs (\x y str -> drawNode x y (toString str) []) argNames)

                children =
                    drawNode leftX newY (toString fName) []
                        ++ args
                        ++ drawSubTree rightX newY body
            in
            edges ++ drawNode xMid y "SetFun" children

        Fun argNames body env ->
            let
                edges =
                    drawEdges xMid y totalWidth 3

                wArgs =
                    List.length argNames * List.foldl (\arg acc -> Basics.max acc <| nodeWidth arg) 0 argNames

                argsEdges =
                    drawEdges leftX newY wArgs (List.length argNames)

                args =
                    argsEdges
                        ++ (drawNode leftX newY "ArgNames" <| makeChildren leftX (nextY newY) wArgs (\x y str -> drawNode x y (toString str) []) argNames)

                children =
                    args
                        ++ drawSubTree xMid newY body
                        ++ drawNode rightX newY (toString env) []
            in
            edges ++ drawNode xMid y "Fun" children

        Apply fName args ->
            let
                edges =
                    drawEdges xMid y totalWidth 2

                wArgs =
                    List.length args * List.foldl (\arg acc -> Basics.max acc <| maxTreeWidth arg) 0 args

                argsEdges =
                    drawEdges rightX newY wArgs (List.length args)

                argsTree =
                    argsEdges
                        ++ (drawNode rightX newY "Args" <| makeChildren rightX (nextY newY) wArgs drawSubTree args)

                children =
                    drawNode leftX newY (toString fName) []
                        ++ argsTree
            in
            edges ++ drawNode xMid y "Apply" children

        Seq exprList ->
            let
                edges =
                    drawEdges xMid y totalWidth (List.length exprList)

                wChildren =
                    List.length exprList * List.foldl (\expr acc -> Basics.max acc <| maxTreeWidth expr) 0 exprList

                children =
                    makeChildren xMid newY wChildren drawSubTree exprList
            in
            edges ++ drawNode xMid y "Seq" children

        Error str ->
            drawNode xMid y ("ERROR: " ++ str) []


drawEdges : Int -> Int -> Int -> Int -> List (Svg msg)
drawEdges parentX parentY w numChildren =
    let
        childrenPos =
            childrenPositions parentX parentY w numChildren

        edgePositions =
            List.map ((,) ( parentX, parentY )) childrenPos
    in
    List.map drawEdge edgePositions


drawBinOp : Int -> Int -> Int -> Int -> String -> List Expr -> List (Svg msg)
drawBinOp x y newY w name childrenList =
    let
        edges =
            drawEdges x y w (List.length childrenList)

        children =
            makeChildren x newY w drawSubTree childrenList
    in
    edges ++ drawNode x y name children


childrenPositions : Int -> Int -> Int -> Int -> List Position
childrenPositions parentX parentY w numChildren =
    let
        leftX =
            parentX - w // 2

        childY =
            nextY parentY

        diff =
            w // (numChildren - 1)

        childrenXs =
            Tuple.second <| List.foldl (\_ ( x, acc ) -> ( x + diff, acc ++ [ x + diff ] )) ( leftX, [ leftX ] ) (List.range 2 numChildren)
    in
    List.map (\x -> ( x, childY )) childrenXs


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
    50

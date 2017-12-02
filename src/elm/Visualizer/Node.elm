module Visualizer.Node exposing (..)

import SimpleAST exposing (Expr(..))
import Svg exposing (..)
import Svg.Attributes as Attrs exposing (..)
import Visualizer.Helpers.Width exposing (marginBetween, maxTreeWidth, nodeWidth, treeWidth)


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
            treeWidth tree

        newY =
            nextY y

        leftX =
            xMid - totalWidth // 2

        -- rightX =
        --     leftX + totalWidth
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

        Add expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Add" [ expr1, expr2 ]

        Mul expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Mul" [ expr1, expr2 ]

        Sub expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Sub" [ expr1, expr2 ]

        LessThan expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "LessThan" [ expr1, expr2 ]

        BiggerThan expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "BiggerThan" [ expr1, expr2 ]

        Equal expr1 expr2 ->
            drawBinOp xMid y newY totalWidth "Equal" [ expr1, expr2 ]

        If bool eThen eElse ->
            let
                wBool =
                    treeWidth bool

                wThen =
                    treeWidth eThen

                wElse =
                    treeWidth eElse

                edges =
                    drawEdges xMid y totalWidth [ wBool, wThen, wElse ]

                children =
                    makeChildren xMid newY totalWidth drawSubTree treeWidth [ bool, eThen, eElse ]
            in
            edges ++ drawNode xMid y "If" children

        SetVar varName body ->
            let
                nameWidth =
                    nodeWidth (toString varName)

                bodyWidth =
                    treeWidth body

                nameX =
                    leftX + nameWidth // 2

                bodyX =
                    nameX + nameWidth // 2 + bodyWidth // 2

                edges =
                    drawEdges xMid y totalWidth [ nameWidth, bodyWidth ]

                children =
                    drawNode nameX newY (toString varName) []
                        ++ drawSubTree bodyX newY body
            in
            edges ++ drawNode xMid y "SetVar" children

        SetFun fName argNames body ->
            let
                wName =
                    nodeWidth fName

                -- wArg =
                --     (+) (marginBetween // 2) << nodeWidth
                wArgs =
                    List.map nodeWidth argNames

                totalWidthArgs =
                    List.sum wArgs + List.length argNames * marginBetween

                wBody =
                    treeWidth body

                nameX =
                    xMid - (totalWidth // 2) + (wName // 2)

                argsX =
                    nameX + wName // 2 + totalWidthArgs // 2 + marginBetween

                bodyX =
                    argsX + totalWidthArgs // 2 + wBody // 2 + marginBetween

                edges =
                    drawEdgesGivenXs xMid y totalWidth [ nameX, argsX, bodyX ]

                argsEdges =
                    drawEdges argsX newY totalWidthArgs wArgs

                args =
                    argsEdges
                        ++ (drawNode argsX newY "ArgNames" <| makeChildren argsX (nextY newY) totalWidthArgs (\x y str -> drawNode x y (toString str) []) nodeWidth argNames)

                children =
                    drawNode nameX newY (toString fName) []
                        ++ args
                        ++ drawSubTree bodyX newY body
            in
            edges ++ drawNode xMid y "SetFun" children

        Fun argNames body env ->
            let
                wArg =
                    (+) (marginBetween // 2) << nodeWidth

                wArgs =
                    List.map wArg argNames

                totalWidthArgs =
                    List.sum wArgs

                wBody =
                    treeWidth body

                wEnv =
                    nodeWidth (toString env)

                argsX =
                    xMid - (totalWidth // 2) + (totalWidthArgs // 2)

                bodyX =
                    argsX + (totalWidthArgs // 2) + (wBody // 2) + marginBetween

                envX =
                    bodyX + wBody + wEnv // 2 + marginBetween

                edges =
                    drawEdgesGivenXs xMid y totalWidth [ argsX, bodyX, envX ]

                argsEdges =
                    drawEdges argsX newY totalWidth wArgs

                args =
                    argsEdges
                        ++ (drawNode argsX newY "ArgNames" <| makeChildren argsX (nextY newY) totalWidthArgs (\x y str -> drawNode x y (toString str) []) nodeWidth argNames)

                children =
                    args
                        ++ drawSubTree bodyX newY body
                        ++ drawNode envX newY (toString env) []
            in
            edges ++ drawNode xMid y "Fun" children

        Apply fName args ->
            let
                argWidths =
                    List.map treeWidth args

                totalWidthArgs =
                    List.sum argWidths

                nameWidth =
                    nodeWidth fName

                edges =
                    drawEdges xMid y totalWidth [ nameWidth, totalWidthArgs ]

                leftX =
                    xMid - (totalWidth // 2) + nameWidth // 2

                rightX =
                    leftX + nameWidth + totalWidthArgs // 2

                argsEdges =
                    drawEdges rightX newY totalWidthArgs argWidths

                argsTree =
                    argsEdges
                        ++ (drawNode rightX newY "Args" <| makeChildren rightX (nextY newY) totalWidthArgs drawSubTree treeWidth args)

                children =
                    drawNode leftX newY (toString fName) []
                        ++ argsTree
            in
            edges ++ drawNode xMid y "Apply" children

        Seq exprList ->
            let
                childrenWidths =
                    List.map treeWidth exprList

                edges =
                    drawEdges xMid y totalWidth childrenWidths

                children =
                    makeChildren xMid newY totalWidth drawSubTree treeWidth exprList
            in
            edges ++ drawNode xMid y "Seq" children

        Error str ->
            drawNode xMid y ("ERROR: " ++ str) []


drawEdgesGivenXs : Int -> Int -> Int -> List Int -> List (Svg msg)
drawEdgesGivenXs parentX parentY totalWidth childrenXs =
    let
        childrenPos =
            childrenPositions parentX parentY totalWidth childrenXs

        edgePositions =
            List.map ((,) ( parentX, parentY )) childrenPos
    in
    List.map drawEdge edgePositions


drawEdges : Int -> Int -> Int -> List Int -> List (Svg msg)
drawEdges parentX parentY totalWidth childrenWidths =
    let
        xs =
            childrenXs parentX totalWidth childrenWidths
    in
    drawEdgesGivenXs parentX parentY totalWidth xs


childrenPositions : Int -> Int -> Int -> List Int -> List Position
childrenPositions parentX parentY w childrenXs =
    let
        childY =
            nextY parentY
    in
    List.map (\x -> ( x, childY )) childrenXs


drawBinOp : Int -> Int -> Int -> Int -> String -> List Expr -> List (Svg msg)
drawBinOp x y newY w name childrenList =
    let
        childrenWidths =
            List.map treeWidth childrenList

        edges =
            drawEdges x y w childrenWidths

        children =
            makeChildren x newY w drawSubTree treeWidth childrenList
    in
    edges ++ drawNode x y name children


childrenXs : Int -> Int -> List Int -> List Int
childrenXs parentX w childrenWidths =
    let
        leftX =
            parentX - w // 2

        childrenXs =
            Tuple.second <| List.foldl (\w ( curX, acc ) -> ( curX + w, acc ++ [ curX + w // 2 + marginBetween ] )) ( leftX - 2 * marginBetween, [] ) childrenWidths
    in
    childrenXs


makeChildren : Int -> Int -> Int -> (Int -> Int -> a -> List (Svg msg)) -> (a -> Int) -> List a -> List (Svg msg)
makeChildren x y w drawFunction widthFunction childrenExprs =
    let
        numChildren =
            List.length childrenExprs

        childrenWidths =
            List.map widthFunction childrenExprs

        xs =
            childrenXs x w childrenWidths

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

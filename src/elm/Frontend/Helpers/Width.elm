module Frontend.Helpers.Width exposing (..)

import Backend.Parser.AST exposing (Expr(..))


treeWidth : Expr -> Int
treeWidth expr =
    case expr of
        Num num ->
            maximum nodeWidth [ "Num", toString num ]

        Var str ->
            maximum nodeWidth [ "Var", str ]

        Add expr1 expr2 ->
            marginBetween + treeWidth expr1 + treeWidth expr2

        Mul expr1 expr2 ->
            marginBetween + treeWidth expr1 + treeWidth expr2

        Sub expr1 expr2 ->
            marginBetween + treeWidth expr1 + treeWidth expr2

        LessThan expr1 expr2 ->
            marginBetween + treeWidth expr1 + treeWidth expr2

        BiggerThan expr1 expr2 ->
            marginBetween + treeWidth expr1 + treeWidth expr2

        Equal expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2 + marginBetween

        If boolExpr expr1 expr2 ->
            2 * marginBetween + (List.sum <| List.map treeWidth [ boolExpr, expr1, expr2 ])

        SetVar var body ->
            marginBetween + nodeWidth var + treeWidth body

        SetFun fName argNamesList body ->
            let
                argsWidth =
                    List.sum <| List.map ((+) marginBetween << nodeWidth << toString) argNamesList
            in
            2 * marginBetween + argsWidth + nodeWidth fName + treeWidth body

        Fun argNamesList body env ->
            let
                argsWidth =
                    List.sum <| List.map ((+) marginBetween << nodeWidth << toString) argNamesList
            in
            marginBetween + argsWidth + nodeWidth "Env" + treeWidth body

        Lambda lamName body ->
            marginBetween + nodeWidth lamName + treeWidth body

        ApplyFun fName argList ->
            let
                argsWidth =
                    List.sum <| List.map treeWidth argList
            in
            marginBetween + nodeWidth fName + argsWidth

        ApplyLam lambda arg ->
            marginBetween + treeWidth lambda + treeWidth arg

        Seq exprList ->
            let
                margin =
                    (List.length exprList - 1) * marginBetween
            in
            margin + (List.sum <| List.map treeWidth exprList)

        Error str ->
            nodeWidth str


nodeWidth : String -> Int
nodeWidth name =
    String.length name * wFACTOR + 10


maximum : (a -> Int) -> List a -> Int
maximum widthFunction list =
    List.foldl (\element acc -> Basics.max (widthFunction element) acc) 0 list


marginBetween : Int
marginBetween =
    35


wFACTOR : Int
wFACTOR =
    11

module Visualizer.Helpers.Width exposing (..)

import SimpleAST exposing (Expr(..))


maxTreeWidth : Expr -> Int
maxTreeWidth expr =
    case expr of
        Num num ->
            maximum nodeWidth [ "Num", toString num ]

        Var str ->
            maximum nodeWidth [ "Var", str ]

        -- Neg expr ->
        --     maxTreeWidth expr
        Add expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        Mul expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        Sub expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        LessThan expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        BiggerThan expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        Equal expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        If boolExpr expr1 expr2 ->
            3 * maximum maxTreeWidth [ boolExpr, expr1, expr2 ]

        SetVar var body ->
            2 * Basics.max (nodeWidth var) (maxTreeWidth body)

        SetFun fName argNamesList body ->
            let
                argsWidth =
                    List.sum <| List.map nodeWidth argNamesList
            in
            3 * Basics.max (Basics.max argsWidth <| nodeWidth fName) (maxTreeWidth body)

        Fun argNamesList body env ->
            let
                argsWidth =
                    List.sum <| List.map nodeWidth argNamesList
            in
            3 * Basics.max (Basics.max argsWidth <| nodeWidth <| toString env) (maxTreeWidth body)

        Apply fName argList ->
            let
                argsWidth =
                    List.sum <| List.map maxTreeWidth argList
            in
            2 * Basics.max (nodeWidth fName) argsWidth

        Seq exprList ->
            List.length exprList * maximum maxTreeWidth exprList

        Error str ->
            nodeWidth str


treeWidth : Expr -> Int
treeWidth expr =
    case expr of
        Num num ->
            maximum nodeWidth [ "Num", toString num ]

        Var str ->
            maximum nodeWidth [ "Var", str ]

        Add expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2

        Mul expr1 expr2 ->
            treeWidth expr1 + treeWidth expr2

        Sub expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        LessThan expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        BiggerThan expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        Equal expr1 expr2 ->
            2 * maximum maxTreeWidth [ expr1, expr2 ]

        If boolExpr expr1 expr2 ->
            3 * maximum maxTreeWidth [ boolExpr, expr1, expr2 ]

        SetVar var body ->
            nodeWidth var + treeWidth body

        SetFun fName argNamesList body ->
            let
                argsWidth =
                    List.sum <| List.map ((+) marginBetween << nodeWidth) argNamesList
            in
            2 * marginBetween + argsWidth + nodeWidth fName + treeWidth body

        Fun argNamesList body env ->
            let
                argsWidth =
                    List.sum <| List.map ((+) marginBetween << nodeWidth) argNamesList
            in
            argsWidth + nodeWidth (toString env) + treeWidth body

        Apply fName argList ->
            let
                argsWidth =
                    List.sum <| List.map treeWidth argList
            in
            nodeWidth fName + argsWidth

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

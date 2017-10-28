module SimpleParser exposing (parse)

import SimpleAST exposing (..)
import List exposing (..)
import String exposing (words)
import Stack exposing (..)


parse : String -> Expr
parse str =
    let
        ( exprStack, opStack ) =
            words str
                |> Just
                |> parseExpr Stack.initialise Stack.initialise
    in
        buildAST exprStack opStack


buildAST : ExprStack -> OpStack -> Expr
buildAST exprStack opStack =
    let
        ( mFirstExpr, exprStack1 ) =
            Stack.pop exprStack

        ( mSecondExpr, exprStack2 ) =
            Stack.pop exprStack1

        ( mThirdExpr, exprStack3 ) =
            Stack.pop exprStack2
    in
        case Stack.pop opStack of
            ( Just (BinOp op), opStack1 ) ->
                let
                    newExprStack =
                        Stack.push (applyBinOp op mFirstExpr mSecondExpr) exprStack2
                in
                    buildAST newExprStack opStack1

            ( Just (UnOp op), opStack1 ) ->
                let
                    newExprStack =
                        Stack.push (applyUnOp op mFirstExpr) exprStack1
                in
                    buildAST newExprStack opStack1

            ( Just (IfOp op), opStack1 ) ->
                let
                    newExprStack =
                        Stack.push (applyIfOp op mFirstExpr mSecondExpr mThirdExpr) exprStack3
                in
                    buildAST newExprStack opStack1

            ( Nothing, _ ) ->
                case Stack.top exprStack of
                    Just expr ->
                        expr

                    Nothing ->
                        Error "Expression stack was empty at the end"


applyBinOp : (Expr -> Expr -> Expr) -> Maybe Expr -> Maybe Expr -> Expr
applyBinOp op mLeft mRight =
    case ( mLeft, mRight ) of
        ( Just left, Just right ) ->
            op left right

        ( _, _ ) ->
            Error "TODO fix applyBinOp"


applyUnOp : (Expr -> Expr) -> Maybe Expr -> Expr
applyUnOp op mExpr =
    case mExpr of
        Just expr ->
            op expr

        Nothing ->
            Error "TODO fix applyUnOp"


applyIfOp : (Expr -> Expr -> Expr -> Expr) -> Maybe Expr -> Maybe Expr -> Maybe Expr -> Expr
applyIfOp op mFirst mSecond mThird =
    case ( mFirst, mSecond, mThird ) of
        ( Just first, Just second, Just third ) ->
            op first second third

        ( _, _, _ ) ->
            Error "TODO fix applyBinOp"


parseExpr : ExprStack -> OpStack -> Maybe (List String) -> ( ExprStack, OpStack )
parseExpr exprStack opStack mStrList =
    case mStrList of
        Nothing ->
            ( exprStack, opStack )

        -- finished reading strings, return to calculate stacks
        Just strList ->
            case head strList of
                Just "+" ->
                    parseExpr exprStack (push (BinOp Add) opStack) (tail strList)

                Just "*" ->
                    parseExpr exprStack (push (BinOp Mul) opStack) (tail strList)

                Just "-" ->
                    parseExpr exprStack (push (BinOp Sub) opStack) (tail strList)

                Just "if" ->
                    parseExpr exprStack (push (IfOp If) opStack) (tail strList)

                Just a ->
                    case String.toInt a of
                        Ok num ->
                            parseExpr (push (Num num) exprStack) opStack (tail strList)

                        Err _ ->
                            parseExpr (push (Var a) exprStack) opStack (tail strList)

                --Just "+" ->
                _ ->
                    ( exprStack, opStack )


type alias ExprStack =
    Stack Expr


type alias OpStack =
    Stack Op


type Op
    = BinOp (Expr -> Expr -> Expr)
    | UnOp (Expr -> Expr)
    | IfOp (Expr -> Expr -> Expr -> Expr)

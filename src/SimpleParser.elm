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
applyIfOp op mThird mSecond mFirst =
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

        Just strList ->
            case head strList of
                Just "+" ->
                    parseBinOp exprStack opStack Add PAdd (tail strList)

                --parseExpr exprStack (push (BinOp Add) opStack) (tail strList)
                Just "*" ->
                    parseBinOp exprStack opStack Mul PMul (tail strList)

                --parseExpr exprStack (push (BinOp Mul) opStack) (tail strList)
                Just "-" ->
                    parseBinOp exprStack opStack Sub PSub (tail strList)

                Just "if" ->
                    parseExpr exprStack (push (IfOp If) opStack) (tail strList)

                Just "then" ->
                    parseExpr exprStack opStack (tail strList)

                Just "else" ->
                    parseExpr exprStack opStack (tail strList)

                Just a ->
                    case String.toInt a of
                        Ok num ->
                            parseExpr (push (Num num) exprStack) opStack (tail strList)

                        Err _ ->
                            parseExpr (push (Var a) exprStack) opStack (tail strList)

                _ ->
                    ( exprStack, opStack )


parseBinOp : ExprStack -> OpStack -> (Expr -> Expr -> Expr) -> PrecedenceType -> Maybe (List String) -> ( ExprStack, OpStack )
parseBinOp exprStack opStack op pType mRest =
    let
        ( leftExpr, exprStack1 ) =
            Stack.pop exprStack

        mNextOpType =
            lookaheadOp mRest

        ( exprStackRest, opStackRest ) =
            parseExpr Stack.initialise Stack.initialise mRest

        ( mBottom, exprStackRest1 ) =
            popBottom exprStackRest

        newExpr =
            applyBinOp op leftExpr mBottom

        mergedExprStack =
            Stack.mergeStacks (Stack.push newExpr exprStack1) exprStackRest1

        mergedOpStack =
            Stack.mergeStacks opStack opStackRest
    in
        if hasHigherPrecedence pType mNextOpType then
            ( mergedExprStack, mergedOpStack )
        else
            parseExpr exprStack (Stack.push (BinOp op) opStack) mRest


popBottom : ExprStack -> ( Maybe Expr, ExprStack )
popBottom stack =
    let
        stackList =
            Stack.toList stack

        ( _, mLast, init ) =
            foldr
                (\x ( isLast, last, list ) ->
                    if isLast then
                        ( False, (Just x), list )
                    else
                        ( False, last, x :: list )
                )
                ( True, Nothing, [] )
                stackList
    in
        ( mLast, Stack.listToStack init )


lookaheadOp : Maybe (List String) -> Maybe PrecedenceType
lookaheadOp mList =
    case mList of
        Nothing ->
            Nothing

        Just list ->
            case head list of
                Nothing ->
                    Nothing

                Just "(" ->
                    Just PParens

                Just ")" ->
                    Just PParens

                Just "*" ->
                    Just PMul

                Just "+" ->
                    Just PAdd

                Just "-" ->
                    Just PSub

                Just "if" ->
                    Just PIf

                Just _ ->
                    lookaheadOp (tail list)


hasHigherPrecedence : PrecedenceType -> Maybe PrecedenceType -> Bool
hasHigherPrecedence p1 mp2 =
    case mp2 of
        Just p2 ->
            precedenceNumber p1 <= precedenceNumber p2

        Nothing ->
            False



--highest precedence == 1, then increasing number for decreasing precedence


precedenceNumber : PrecedenceType -> Int
precedenceNumber pType =
    case pType of
        PParens ->
            1

        --PNeg -> 2
        -- PApply -> 3 TODO Fix
        PMul ->
            2

        PAdd ->
            3

        PSub ->
            3

        -- P "\<"
        PIf ->
            4

        _ ->
            5


type PrecedenceType
    = PParens
      --| PNeg
      --| PApply
    | PMul
    | PAdd
    | PSub
      --| P "\<"
    | PIf
      --| PLet
      --| PLetFun
      --| PLambda
    | PVar
    | PNum


type alias ExprStack =
    Stack Expr


type alias OpStack =
    Stack Op


type Op
    = BinOp (Expr -> Expr -> Expr)
    | UnOp (Expr -> Expr)
    | IfOp (Expr -> Expr -> Expr -> Expr)

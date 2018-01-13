module Backend.Parser.Parser exposing (parse)

import Backend.Parser.AST exposing (..)
import Backend.Parser.Helpers.Lookahead exposing (..)
import Backend.Parser.Helpers.Stack as Stack exposing (..)
import Backend.Parser.Tokenizer as Tokenizer exposing (tokenize)
import Char


parse : String -> Expr
parse str =
    let
        exprs =
            str
                |> String.split ";"
                |> List.filter (not << String.isEmpty)
                -- filter: remove last empty str if semicolon in the end
                |> List.map (parseLine << tokenize << String.toList)
    in
    case exprs of
        [ expr ] ->
            expr

        xs ->
            Seq exprs


parseLine : List String -> Expr
parseLine =
    uncurry buildAST << parseExpr Stack.initialise Stack.initialise


parseParens : List String -> ( Expr, List String )
parseParens xs =
    case xs of
        "(" :: rest ->
            let
                ( insideParens, rest2 ) =
                    readTil ")" rest
            in
            ( parseLine insideParens, rest2 )

        _ ->
            ( Error "Expected parentheses expression, but found no surrounding parenthesis", [] )


parseExpr : ExprStack -> OpStack -> List String -> ( ExprStack, OpStack )
parseExpr exprStack opStack strList =
    case strList of
        "+" :: rest ->
            parseBinOp exprStack opStack Add PAdd rest

        -- "-" -> Not supporting unary minus
        "*" :: rest ->
            parseBinOp exprStack opStack Mul PMul rest

        "-" :: rest ->
            parseBinOp exprStack opStack Sub PLast rest

        "<" :: rest ->
            parseBinOp exprStack opStack LessThan PLast rest

        ">" :: rest ->
            parseBinOp exprStack opStack BiggerThan PLast rest

        "==" :: rest ->
            parseBinOp exprStack opStack Equal PLast rest

        -- {- Lambda application: (\varName -> some expression) (arg) -}
        "(" :: "\\" :: varName :: rest ->
            let
                ( bodyStrs, rest1 ) =
                    readTil ")" rest

                lambda =
                    parseLambda varName bodyStrs

                ( arg, rest2 ) =
                    parseLambdaArg rest1

                lambdaApp =
                    ApplyLam lambda arg
            in
            if List.isEmpty rest1 then
                ( push lambda exprStack, opStack )
            else
                parseExpr (push lambdaApp exprStack) opStack rest2

        {- Lambda value: \varName -> some expression -}
        "\\" :: varName :: rest ->
            let
                lambda =
                    parseLambda varName rest
            in
            ( push lambda exprStack, opStack )

        "(" :: rest ->
            let
                ( insideParens, rest1 ) =
                    readTil ")" rest

                parensExpr =
                    parseLine insideParens
            in
            parseExpr (Stack.push parensExpr exprStack) opStack rest1

        "if" :: rest ->
            let
                ( condExpr, rest1 ) =
                    parseParens rest

                thenStrs =
                    skip "then" rest1

                ( thenExpr, rest2 ) =
                    parseParens thenStrs

                elseStrs =
                    skip "else" rest2
            in
            Debug.log ("Rest: " ++ toString rest) parseExpr exprStack (push (UnOp <| If condExpr thenExpr) opStack) elseStrs

        {- function f (a, b, c) = something -}
        "function" :: fName :: "(" :: rest ->
            let
                ( argNames, rest1 ) =
                    readTil ")" rest

                argNamesSplitOnComma =
                    List.concat <| splitOnComma argNames
            in
            if validName fName then
                case rest1 of
                    "=" :: rest2 ->
                        parseExpr exprStack (push (SetOp <| SetFun fName argNamesSplitOnComma) opStack) rest2

                    xs ->
                        ( push (Error <| "function declaration must have '=' before body, was: " ++ toString xs) exprStack, opStack )
            else
                ( push (Error <| "Invalid function name: " ++ fName) exprStack, opStack )

        "function" :: _ ->
            ( push (Error "syntax error for function declaration") exprStack, opStack )

        "set" :: vName :: "=" :: rest ->
            if validName vName then
                parseExpr exprStack (push (SetOp <| SetVar vName) opStack) rest
            else
                ( push (Error <| "Invalid variable name: " ++ vName) exprStack, opStack )

        "set" :: _ ->
            ( push (Error "syntax error for set-variable") exprStack, opStack )

        {- Apply: fName (args) -}
        fName :: "(" :: rest ->
            let
                ( argStrs, rest1 ) =
                    readTil ")" rest

                argsSplitOnComma =
                    splitOnComma argStrs

                exprArgs =
                    List.map (parse << String.join " ") argsSplitOnComma
            in
            parseExpr (push (Apply fName exprArgs) exprStack) opStack rest1

        a :: rest ->
            let
                atom =
                    parseAtom a
            in
            parseExpr (push atom exprStack) opStack rest

        [] ->
            ( exprStack, opStack )


splitOnComma : List String -> List (List String)
splitOnComma strs =
    let
        ( arg, rest ) =
            readTil "," strs
    in
    case rest of
        [] ->
            [ arg ]

        xs ->
            arg :: splitOnComma rest


parseBinOp : ExprStack -> OpStack -> (Expr -> Expr -> Expr) -> PrecedenceType -> List String -> ( ExprStack, OpStack )
parseBinOp exprStack opStack op pType rest =
    let
        nextOpType =
            lookaheadOp rest

        ( exprStackRest, opStackRest ) =
            parseExpr Stack.initialise Stack.initialise rest

        ( mLastExpr, exprStack1 ) =
            pop exprStack

        ( mNextExpr, exprStackRest1 ) =
            popBottom exprStackRest
    in
    if not (hasHigherPrecedence nextOpType pType) then
        let
            newExpr =
                applyBinOp op mLastExpr mNextExpr

            mergedExprStack =
                Stack.mergeStacks (Stack.push newExpr exprStack1) exprStackRest1

            mergedOpStack =
                Stack.mergeStacks opStack opStackRest
        in
        ( mergedExprStack, mergedOpStack )
    else
        parseExpr exprStack (Stack.push (BinOp op) opStack) rest


parseAtom : String -> Expr
parseAtom atom =
    case String.toInt atom of
        Ok num ->
            Num num

        Err _ ->
            Var atom


parseLambda : String -> List String -> Expr
parseLambda varName rest =
    if validName varName then
        case rest of
            "->" :: rest1 ->
                let
                    body =
                        parse <| String.join " " rest1
                in
                Lambda varName body

            xs ->
                Error <| "Lambda expression must have '->' before body, was: " ++ toString xs
    else
        Error <| "Invalid lambda variable: " ++ varName


parseLambdaArg : List String -> ( Expr, List String )
parseLambdaArg strs =
    case strs of
        "(" :: rest ->
            let
                ( insideParens, rest1 ) =
                    readTil ")" rest

                arg =
                    parseLine insideParens
            in
            ( arg, rest1 )

        x :: rest ->
            let
                atom =
                    parseAtom x
            in
            ( atom, rest )

        _ ->
            ( Error "Lambda argument was empty", [] )


buildAST : ExprStack -> OpStack -> Expr
buildAST exprStack opStack =
    let
        ( mLastExpr, exprStack1 ) =
            Stack.pop exprStack

        ( mSndLastExpr, exprStack2 ) =
            Stack.pop exprStack1

        ( mThdLastExpr, exprStack3 ) =
            Stack.pop exprStack2
    in
    case mLastExpr of
        Just (Error str) ->
            Error <| "Parse error: " ++ str

        _ ->
            case Stack.pop opStack of
                ( Just (BinOp op), opStack1 ) ->
                    let
                        newExprStack =
                            Stack.push (applyBinOp op mSndLastExpr mLastExpr) exprStack2
                    in
                    buildAST newExprStack opStack1

                ( Just (UnOp op), opStack1 ) ->
                    let
                        newExprStack =
                            Stack.push (applyUnOp op mLastExpr) exprStack1
                    in
                    buildAST newExprStack opStack1

                ( Just (SetOp op), opStack1 ) ->
                    case mLastExpr of
                        Just lastExpr ->
                            let
                                newExprStack =
                                    Stack.push (op lastExpr) exprStack1
                            in
                            buildAST newExprStack opStack1

                        Nothing ->
                            Error "Expression stack is empty when trying to make a Set-node"

                ( Nothing, _ ) ->
                    case Stack.top exprStack of
                        Just expr ->
                            expr

                        Nothing ->
                            Error "Expression stack was empty at the end, have nothing to return"


applyBinOp : (Expr -> Expr -> Expr) -> Maybe Expr -> Maybe Expr -> Expr
applyBinOp op mLeft mRight =
    case ( mLeft, mRight ) of
        ( Just left, Just right ) ->
            op left right

        ( a, b ) ->
            Error <| "Binary operator " ++ toString op ++ " needs two arguments, but got: (" ++ toString a ++ ", " ++ toString b


applyUnOp : (Expr -> Expr) -> Maybe Expr -> Expr
applyUnOp op mExpr =
    case mExpr of
        Just expr ->
            op expr

        Nothing ->
            Error <| "Unary operator " ++ toString op ++ " needs one argument, but arguments was Nothing."


keywords : List String
keywords =
    [ "function", "set" ]


validName : String -> Bool
validName name =
    let
        firstChar =
            String.left 1 name

        validFirstChar =
            String.all Char.isLower firstChar

        notKeyword s =
            not <| List.member s keywords
    in
    notKeyword name && validFirstChar && String.all Tokenizer.isVariableChar name

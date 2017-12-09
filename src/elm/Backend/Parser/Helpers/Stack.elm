module Backend.Parser.Helpers.Stack exposing (..)

import Backend.Parser.AST exposing (Expr)


type Stack a
    = Stack (List a)


type alias ExprStack =
    Stack Expr


type alias OpStack =
    Stack Op


type Op
    = BinOp (Expr -> Expr -> Expr)
    | UnOp (Expr -> Expr)
    | IfOp (Expr -> Expr -> Expr)
    | SetOp (Expr -> Expr)


{-| Initialise an empty stack.
-}
initialise : Stack a
initialise =
    Stack []


{-| Convert a Stack type to a list data type
-}
toList : Stack a -> List a
toList (Stack stack) =
    stack


{-| Pushes an item onto the stack and returns the new stack. The item must be of the same type as the stack.
-}
push : a -> Stack a -> Stack a
push item (Stack stack) =
    Stack (item :: stack)


{-| Returns the top element of the stack without removing it.
-}
top : Stack a -> Maybe a
top (Stack stack) =
    List.head stack


{-| Removes the item at the top of the stack and returns it as the first item of a tuple.
-}
pop : Stack a -> ( Maybe a, Stack a )
pop (Stack stack) =
    case stack of
        [] ->
            ( Nothing, Stack [] )

        head :: tail ->
            ( Just head, Stack tail )


popBottom : Stack a -> ( Maybe a, Stack a )
popBottom stack =
    let
        stackList =
            toList stack

        ( _, mLast, init ) =
            List.foldr
                (\x ( isLast, last, list ) ->
                    if isLast then
                        ( False, Just x, list )
                    else
                        ( False, last, x :: list )
                )
                ( True, Nothing, [] )
                stackList
    in
    ( mLast, listToStack init )


listToStack : List a -> Stack a
listToStack list =
    Stack list


mergeStacks : Stack a -> Stack a -> Stack a
mergeStacks (Stack stack1) (Stack stack2) =
    Stack (List.append stack2 stack1)

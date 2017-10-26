module SimpleTypeAST exposing (..)


type alias Args =
    List ExprAst


type alias Loc =
    String


type Type
    = Fun (List Type) Type -- function with list of params, and return type
    | Int


type Param
    = Param Type String


type ExprAst
    = Num Int
    | Apply ExprAst Args
    | If ExprAst ExprAst ExprAst -- cond then else
    | Let String ExprAst
    | LetFun String (List Param) ExprAst ExprAst
    | Builtin String
    | Error String Loc ExprAst

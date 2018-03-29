module Backend.Parser.AST exposing (..)

import Dict exposing (..)


type Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | LessThan Expr Expr
    | BiggerThan Expr Expr
    | Equal Expr Expr
    | If Expr Expr Expr
    | SetVar String Expr -- set a = something
    | SetFun String ArgNames Expr -- function foo ( a, b, c ) = something
    | Fun ArgNames Expr Env
    | Lambda String Expr
    | Apply String Args
    | ApplyLam Expr Expr -- First expr is going to be Lambda
    | Seq (List Expr)
    | Error String


type alias Args =
    List Expr


type alias ArgNames =
    List String


type alias Env =
    Dict String Expr


type Type
    = TNum
    | TFun


type alias State =
    ( Env, Expr )

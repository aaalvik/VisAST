module SimpleAST exposing (..)

import Dict exposing (..)


type Expr
    = InParens Expr
    | Neg Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | LessThan Expr Expr -- | "\<" Expr Expr
    | If Expr Expr Expr
    | Let VarName Expr Expr -- Var = Expr in Expr
    | LetFun FunName ArgNames Expr Expr
    | Apply FunName Args -- removed parens
      -- | Lambda Var Expr -- Var to Expr
    | Var VarName
    | Num Int
    | Error String


type Type
    = Int
    | TFun Type Type -- function from Type to Type


type Param
    = Param Type String


type alias FunName =
    String


type alias VarName =
    String

type alias Args = List Expr

type alias ArgNames =
    List String


type alias NumEnv =
    Dict String Int


type alias FunEnv =
    Dict String Fun


type alias Body =
    Expr


type Fun
    = Fun ArgNames Body Env


type alias Env =
    { numEnv : NumEnv, funEnv : FunEnv }


emptyEnv : Env
emptyEnv =
    { numEnv = Dict.empty, funEnv = Dict.empty }



-- type MyException = UnknownExpression Expr -- Find some supertype?
-- TODO check against regex [a-zA-Z]


validVar : Expr -> Bool
validVar var =
    True



-- type TypeVar
--     = Type String

module SimpleAST exposing (..)


type Expr
    --= InParens Expr
    --= Neg Expr
      --| Apply Expr Expr -- removed parens
    = Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
      -- | "\<" Expr Expr
    | If Expr Expr Expr
      --| Let Var Expr Expr -- Var = Expr in Expr
      --| LetFun TypeVar TypeVar
      -- | Lambda Var Expr -- Var to Expr
    | Var String
    | Num Int
    | Error String


type Type
    = Int
    | Fun Type Type -- function from Type to Type



-- type MyException = UnknownExpression Expr -- Find some supertype?
-- TODO check against regex [a-zA-Z]


validVar : Expr -> Bool
validVar var =
    True


type TypeVar
    = Type String

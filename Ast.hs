module Ast (
    Expr(..),
    exprSize
) where

data Expr =
    -- intliteral: value
      IntLiteral Int
    -- strliteral: value
    | StrLiteral String
    -- boolliteral: value
    | BoolLiteral Bool
    -- var: name
    | Var String
    -- arithmetic operators
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    -- logical operators
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Equals Expr Expr
    | Less Expr Expr
    | LessEqual Expr Expr
    | Greater Expr Expr
    | GreaterEqual Expr Expr
    -- control structures
    -- if: pred then else
    | If Expr Expr Expr
    -- let: x e1 e2 (let x = e1 in e2)
    | Let String Expr Expr
    -- function app: e1 e2
    | App Expr Expr
    deriving (Eq)

instance Show Expr where
    show (IntLiteral i)         = show i
    show (StrLiteral s)         = "\"" ++ s ++ "\""
    show (BoolLiteral b)        =  show b
    show (Var v)                = v
    show (Add e1 e2)            = "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
    show (Subtract e1 e2)       = "(" ++ (show e1) ++ " - " ++ (show e2) ++ ")"
    show (Multiply e1 e2)       = "(" ++ (show e1) ++ " * " ++ (show e2) ++ ")"
    show (Divide e1 e2)         = "(" ++ (show e1) ++ " / " ++ (show e2) ++ ")"
    show (Not e1)               = "!" ++ (show e1)
    show (And e1 e2)            = "(" ++ (show e1) ++ " && " ++ (show e2) ++ ")"
    show (Or e1 e2)             = "(" ++ (show e1) ++ " || " ++ (show e2) ++ ")"
    show (Equals e1 e2)         = "(" ++ (show e1) ++ " == " ++ (show e2) ++ ")"
    show (Less e1 e2)           = "(" ++ (show e1) ++ " < " ++ (show e2) ++ ")"
    show (LessEqual e1 e2)      = "(" ++ (show e1) ++ " <= " ++ (show e2) ++ ")"
    show (Greater e1 e2)        = "(" ++ (show e1) ++ " > " ++ (show e2) ++ ")"
    show (GreaterEqual e1 e2)   = "(" ++ (show e1) ++ " >= " ++ (show e2) ++ ")"
    show (If pred e1 e2)        = "if " ++ (show pred) ++ " then " ++ (show e1) ++ " else " ++ (show e2)

-- return the size of the expression
-- this is used for constraint weighting
exprSize :: Expr -> Int
exprSize (IntLiteral _)         = 1
exprSize (StrLiteral _)         = 1
exprSize (BoolLiteral _)        = 1
exprSize (Var _)                = 1
exprSize (Add e1 e2)            = (exprSize e1) + (exprSize e2) + 1
exprSize (Subtract e1 e2)       = (exprSize e1) + (exprSize e2) + 1
exprSize (Multiply e1 e2)       = (exprSize e1) + (exprSize e2) + 1
exprSize (Divide e1 e2)         = (exprSize e1) + (exprSize e2) + 1
exprSize (Not e1)               = (exprSize e1) + 1
exprSize (And e1 e2)            = (exprSize e1) + (exprSize e2) + 1
exprSize (Or e1 e2)             = (exprSize e1) + (exprSize e2) + 1
exprSize (Equals e1 e2)         = (exprSize e1) + (exprSize e2) + 1
exprSize (Less e1 e2)           = (exprSize e1) + (exprSize e2) + 1
exprSize (LessEqual e1 e2)      = (exprSize e1) + (exprSize e2) + 1
exprSize (Greater e1 e2)        = (exprSize e1) + (exprSize e2) + 1
exprSize (GreaterEqual e1 e2)   = (exprSize e1) + (exprSize e2) + 1
exprSize (If pred e1 e2)        = (exprSize pred) + (exprSize e1) + (exprSize e2) + 1
exprSize (Let var e1 e2)        = (exprSize e1) + (exprSize e2) + 1
exprSize (App e1 e2)            = (exprSize e1) + (exprSize e2)



    

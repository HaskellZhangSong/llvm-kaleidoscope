module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)

zero :: Expr
zero = Function "zero" [] (Float 0)

func :: Expr
func = Function "foo" ["a","b"] (BinaryOp "+" (BinaryOp "+" (BinaryOp "*" (Var "a") (Var "a")) (BinaryOp "*" (BinaryOp "*" (Float 2.0) (Var "a")) (Var "b"))) (BinaryOp "*" (Var "b") (Var "b")))
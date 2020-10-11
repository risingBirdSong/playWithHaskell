data Expr = I Int
          | B Bool           -- boolean constants
          | Add Expr Expr
          | Mul Expr Expr
          | Eq  Expr Expr    -- equality test

eval :: Expr -> Either Int Bool
eval (I n) = Left n
eval (B b) = Right b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- this is a problem 
-- eval (B b) = b is an error because the type signrature is returning an Int but this is return a Bool
-- inference doesnt help

-- but now we get in trouble with since we cant add booleans and the type checker complains
data Expr_a = I Int         -- integer constants
          | Add Expr_a Expr_a -- add two expressions
          | Mul Expr_a Expr_a -- multiply two expressions 
          deriving (Show, Eq, Ord)

eval :: Expr_a -> Int
eval (I n)       = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

myEval = I 7
seven = eval myEval -- 7
myAdd = Add (I 7) (I 7)
fourteen = eval myAdd -- 14
myMult = Mul (I 7) (I 7)
fortyNine = eval myMult -- 49
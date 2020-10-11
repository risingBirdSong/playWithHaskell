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

-- eval_b :: Expr_b -> Either Int Bool
-- eval_b :: Expr_b -> Either Int Bool
eval_b :: Expr_b -> Maybe (Either Int Bool)
eval_b (I_b n)       = Expr_b n
eval_b (B n)       =  Expr_b n 
eval_b (Add_b e1 e2) = eval_b e1 + eval_b e2
eval_b (Mul_b e1 e2) = eval_b e1 * eval_b e2
data Expr_b = I_b Int
          | B Bool           -- boolean constants
          | Add_b Expr_b Expr_b
          | Mul_b Expr_b Expr_b
          | Eq  Expr_b Expr_b    -- equality test
          deriving (Show, Eq, Ord)


evaled_b1 = eval_b (I_b 2 `Add_b` I_b 4) -- 6
-- the problem is the following type checks and even evaluates
valid = B True `Add_b` I_b 3 :: Expr_b -- Add_b (B True) (I_b 3)
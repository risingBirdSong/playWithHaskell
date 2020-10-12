{-# LANGUAGE GADTs #-}

data Expr a where
    I   :: Int  -> Expr Int 
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

added = eval (I 3) + eval (I 3) -- 6
added_a = (I 3) `Add` (I 4) -- 7

data DivisionResult = DivisionByZero | Success Double
    deriving (Show)

safeDivide :: Double -> Double -> DivisionResult
safeDivide x y =
    if y == 0
    then DivisionByZero
    else Success (x / y)

-- Try dividing by zero
safe = safeDivide 9 3 -- Success 3.0
notSafe = safeDivide 11 0 -- DivisionByZero
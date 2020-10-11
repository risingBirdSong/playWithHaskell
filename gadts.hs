data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)
            deriving (Show, Eq, Ord)

-- eval :: Expr a -> a
i :: Int  -> Expr Int 
i = I
b :: Bool -> Expr Bool
b = B
add' :: Expr Int -> Expr Int -> Expr Int
add' = Add
mul' :: Expr Int -> Expr Int -> Expr Int
mul' = Mul

-- i 2 `add'` i 3  ->
-- Add (I 2) (I 3)

-- the previously problematic expression -> no longer type checks! 
wontTypeCheck = b True `add'` i 5
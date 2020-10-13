type Matrix= [[Int]]
type Pos = (Int,Int)
f :: Pos -> Pos -> Matrix
f (h, w) p =  [ [if (y, x) == p then 1 else 0 | x <- [1..w]]
              | y <- [1..h]]

myMatrix h w = [[ x + y -1  | x <- [1..w]] | y <- [1..3]]

-- i :: [Int] 
-- i = (iterate + 1 , 1)

-- myMatrix 3 3 -> [[0,0,0],[0,0,0],[0,0,0]]

-- 1 2 3
-- 2
-- 3
rowMaker1 n k = [ n .. n+k-1 ] : rowMaker1 (n+k) k
-- take 3 (rowMaker1 1 3) 
-- [[1,2,3],[4,5,6],[7,8,9]]
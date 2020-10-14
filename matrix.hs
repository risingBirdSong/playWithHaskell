import GHC.Base (build)
type Matrix= [[Int]]
type Pos = (Int,Int)
testMatrix :: (Enum t, Num t, Num a) => p -> t -> [[a]]
testMatrix h w = [[ 1  | x <- [1..w]] | y <- [1..3]]
f :: Pos -> Pos -> Matrix
f (h, w) p =  [ [if (y, x) == p then 1 else 0 | x <- [1..w]]
              | y <- [1..h]]
-- what about having a stack and popping it off each time to get an incrementing num?


-- i :: [Int] 
-- i = (iterate + 1 , 1)

-- myMatrix 3 3 -> [[0,0,0],[0,0,0],[0,0,0]]

-- 1 2 3
-- 2
-- 3
rowMaker1 n k = [ n .. n+k-1 ] : rowMaker1 (n+k) k
-- take 3 (rowMaker1 1 3) 
-- [[1,2,3],[4,5,6],[7,8,9]]

c = chunksOf 3 [1..9]

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

matrixOf n = [[x + n * (y - 1) | x <- [1..n]] | y <- [1..n]]

mtrx n = zipWith (fmap . (+)) [0,n..] $ replicate n [1..n]


strdata = ['m', 'y', 'e', 'x', 'a', 'm', 'p', 'l', 'e']

strMtrx = [["m", "y", "e"],["x", "a", "m"], ["p", "l", "e"]]
charMtrx = [['m', 'y', 'e'],['x', 'a', 'm'], ['p', 'l', 'e']]


makeMeal = gather [0,2,4,7] strdata -- "meal"

-- solve mtx idxs = gather idxs "myexample"

-- strs mtx = (concat . fltMtrx mtx ) []
fltMtrx :: Foldable t => [t a] -> [a] -> [[a]]
fltMtrx [] [] = []
fltMtrx (xs:xxs) acc = foldr (:) acc xs : fltMtrx (xxs) acc  

gather [] strs = [] 
gather (id:idxs) strs = (strs !! id) : gather idxs strs
gridIndex :: [[Char]] -> [Int] -> String
gridIndex mtx idxs = gather (map (+(-1)) idxs) strs 
  where strs = (concat . fltMtrx mtx) []

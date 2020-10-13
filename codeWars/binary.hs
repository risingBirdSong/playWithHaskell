import Data.Binary
import GHC.Float
import GHC.Float.RealFracMethods
import Data.List
-- gotA = get 
convert :: Double -> Int
convert x = round x

binMult :: [Int]
binMult = (map convert (map (2**) [0,1..]))

toNumber xs =sum ((zipWith (*) binMult (reverse xs)))

toNumber' :: [Int] -> Int
toNumber' = foldl f 0
  where f acc n = 2*acc+n

foldLMinus = foldl (-) 0 [1,2,3,4] -- -10
foldRMinus = foldr (-) 0 [1,2,3,4] -- -2
-- remakeToNumber = foldr 

-- foldr (-) 0 [1,2,3,4]

-- (1 - (2 - (3 - (4 - 0)))) = -2

--   -
--  / \
-- 1   -
--    / \
--   2   -
--      / \
--     3   -
--        / \
--       4   0

-- foldl (-) 0 [1,2,3,4]

-- ((((0 - 1) - 2) - 3) - 4) = -10

--         -
--        / \
--       -   4
--      / \
--     -   3
--    / \
--   -   2
--  / \
-- 0   1
-- i got the order of the acc and cur wrong at first -> wrong answer. but looking at the type sig of Foldl
-- helps, you can see that the first argument is a fn that takes b, the accumulator, first, a is cur.
-- Foldable t => (b -> a -> b) -> b -> t a -> b
remakeToNumber :: [Int] -> Int 
remakeToNumber = foldl f 0 
  where f acc x = 2 * acc + x
  
remakeToNumberLetIn :: [Int] -> Int
remakeToNumberLetIn = let f acc c = 2*acc+c in foldl f 0

remakeToNumberLambda :: [Int] -> Int
remakeToNumberLambda = foldl (\acc x -> 2*acc+x) 0 
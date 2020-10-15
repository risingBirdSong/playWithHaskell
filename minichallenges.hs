import qualified Data.Tree as T
import Data.List
-- see if list of strings contain given chars
filtering xs c = filter (==c) xs -- "oo"

contains y xs = any (==y) xs

listContains xxs y = map (contains y) xxs

-- add list of list numbers
add' xs = sum (map (sum) xs)

-- does concat flatten any amount of nesting?

concatAnyNest [] = [] 
concatAnyNest xs = concat xs

data Item a = One a | Many [Item a]

flatten (One x) = [x]
flatten (Many xs) = concat $ map flatten xs

-- infinite type
-- flatten'  x = [x]
-- flatten'  xs = concat $ map flatten xs
firstWorking =  flatten (One 4) -- [4]
ex2 = flatten (Many [(One 3), (One 4)]) -- [3,4]
ex3 = flatten (Many [(One 1), Many [(One 2), Many [One 3, One 4]]]) -- [1,2,3,4]


buildNode x = if 2*x + 1 > 7 then (x, []) else (x, [2*x, 2*x+1])
makeAndShow = putStr $ T.drawTree $ fmap show $ T.unfoldTree buildNode 1
-- 1
-- |
-- +- 2
-- |  |
-- |  +- 4
-- |  |
-- |  `- 5
-- |
-- `- 3
--    |
--    +- 6
--    |
--    `- 7


lrgst [] = Nothing
lrgst xs = Just (head (reverse $ sort xs))

maxed strt xs = foldr max strt xs

max' [] c = c 
max' (x:xs) c
  | x >= c = max' xs x
  | x < c = max' xs c 
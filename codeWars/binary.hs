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
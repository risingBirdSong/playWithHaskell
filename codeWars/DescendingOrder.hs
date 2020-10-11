import Data.List
import Control.DeepSeq
import Debug.Trace
descendingOrder :: Integer -> Integer
descendingOrder = error "todo: descendingOrder"

-- split the numbers into singletons, sort them and concat them

-- "Converting the number into a string is an impaired way of doing things."
triala = sort [1,9,8,2,7,3,6,4,5]

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
shortener 0 = []
shortener x = shortener (x `div` 10) ++ "."

dividInts :: Int -> Int
dividInts x = x `div` 10  

dividFloats :: Float -> Float
dividFloats x = x / 10 

-- d = digs 12345
-- d !! 4
-- :sprint d

d = digs 12345

-- d = digs 1234567
-- take 5 d
-- :sprint d


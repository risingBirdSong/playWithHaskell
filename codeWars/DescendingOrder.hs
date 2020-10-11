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

-- d = digs 12345
-- d !! 4
-- :sprint d

d = digs 12345

-- d = digs 1234567
-- take 5 d
-- :sprint d


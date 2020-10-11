import Data.List as L
import Debug.Trace


-- split the numbers into singletons, sort them and concat them

-- "Converting the number into a string is an impaired way of doing things."
triala = sort [1,9,8,2,7,3,6,4,5]

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fromDigits :: (Num a) => [a] -> a
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

descendingOrder x = reverse . sort . digs $ x

shortener 0 = []
shortener x = shortener (x `div` 10) ++ "<" ++ show (length (show x)) ++ ">"
demoShortener = shortener 13454354354352345345
demoResult = "<1><2><3><4><5><6><7><8><9><10><11><12><13><14><15><16><17><18><19><20>"

dividInts :: Int -> Int
dividInts x = x `div` 10  

dividFloats :: Float -> Float
dividFloats x = x / 10 

-- giveMeInt :: (Num a) => a -> Int
giveMeInt :: (Double) -> Int
giveMeInt x = round x

-- d = digs 12345
-- d !! 4
-- :sprint d

d = digs 12345

-- d = digs 1234567
-- take 5 d
-- :sprint d


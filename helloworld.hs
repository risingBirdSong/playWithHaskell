import Control.Monad  
import Control.Applicative
import System.IO     
import Control.Monad

import Control.Monad.RWS

import Data.Monoid
import Data.List
-- import Data.List.genericLength



withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result 


myAction = (++) <$> getLine <*> getLine

main = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a  

    -- because 4 are not conained, not in functors
-- doesntWork = (+) <$> 4 <*> 4
-- this does work because both vals are in functors
applicative_a = (+) <$> [4] <*>[4] -- [8]
applicative_b = (+) <$> Just 5 <*> Just 5 -- Just 10
-- notice how when you a val and function inside of just, that basically doing the job of <$>
applicative_c = Just (5+) <*> Just 5 -- Just 10

-- I was confused about the type info here of Num and Enum
-- :t [1..3]
-- [1..3] :: (Num a, Enum a) => [a]

-- from ednob
-- b is a type
-- It is constrained, such that b must have an instance of both Num and Enum
-- Everything you can convert to and from an Int is an Enum, iirc

addUpNums :: (Num b, Enum b) => b -> b -> b
addUpNums x y = foldl (+) 0 [x..y]


-- so cool
summedUp :: Int
summedUp = sum [1..10]

myConst = const 3 4
-- >>> const 42 "hello"
-- 42
-- >>> map (const 42) [0..3]
-- [42,42,42,42]

-- interesting qoute
-- "If you want to reduce [design time], you
-- have to stop thinking about something you
-- used to have to think about.” 

oddCheck = odd 5 -- True
sins = map sin [1.01,1.02..2.02]

zippeda = zip [1..10] [10,9..1] -- [(1,10),(2,9),(3,8),(4,7),(5,6),(6,5),(7,4),(8,3),(9,2),(10,1)]

-- a rounded average
average :: (Foldable t) => t Int -> Int
average xs = (sum xs) `div` (length xs)

summed = sum [1..10]
summeda = sum [1.1..10.0]

average_ xs = realToFrac (sum xs) / genericLength xs

-- real to frac 
realA :: Integer
realA = 3
rlToFrac :: Double
rlToFrac = realToFrac realA

avgA = average . take 20 $  [1,4..] -- 29

zipB = zipWith (*) [1..10] [1..10] -- [1,4,9,16,25,36,49,64,81,100]
zipC = zipWith (**) [2.1,2.2..5.0] [5.0,4.9..1.0] -- 

-- [40.841010000000004,47.628964105068675,54.48708984819458,61.234029071053605,67.68992608936989,73.68516593572976,79.06796517257521,83.71042267018665,87.51280236188828,90.40597709674667,92.35210000000048,93.34367932254544,93.40131087450607,92.57036860731965,90.91697128694729,88.52353590769029,85.48420198890096,81.90037095523637,77.87655696470087,73.51669471981101,68.92100000000077,64.18343292965673,59.38977440934806,54.616293855592716,49.92896182158642,45.38314400744025,41.02370297173611,36.88542959713919,32.993726959373916,29.365473577200987]
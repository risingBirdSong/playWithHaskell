import Control.Monad  
import Control.Applicative
import System.IO     

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


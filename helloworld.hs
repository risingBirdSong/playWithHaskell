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
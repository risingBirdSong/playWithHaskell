import Data.List
import Data.Function
testa =  ( head . head . sortBy (compare `on` length) . groupBy (\ x y -> (x == y))) [1,2,2,2,2,2,3,3,9]

getuniqueNumber = ( head . concat . sortBy (compare `on` length) . groupBy (\ x y -> (x == y))) [2,2,2,2,2,9]

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c 

-- from Arc
-- b -> b -> c is the binary function.
-- a -> b is the "preprocessor"

-- I think of on as applying a function to both inputs of a binary function before the binary function.
-- Like a preprocessor.
-- @Eval :t (==) `on` length @[]
-- [a] -> [a] -> Bool
-- So here it's changing == to compare on length instead of contents.
-- a -> b is the "preprocessor"

-- from IFcoltransG
-- a is the type of the initial two values, b is the type after you run them through a function, and c is the type after you combine them with the binary function.
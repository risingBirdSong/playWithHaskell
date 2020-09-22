import Data.List
import Data.Function
-- In a classic .hs source file, the relevant pragma must be included:
{-#  LANGUAGE TypeApplications  #-}
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

-- i was getting error trying in console
-- :t (==) `on` length @[]
-- Pattern syntax in expression context: length@[]
-- Did you mean to enable TypeApplications?
-- i was trying to fix it in the global yaml file adding
-- :set -XTypeApplications
-- not working, but found you just type that in the console, and now working!
-- https://stackoverflow.com/questions/30326249/what-does-mean-in-haskell/30326349

-- and what is @ solving?
-- :t (==) `on` length []  
-- error 
-- Couldn't match expected type `a -> a0' with actual type `Int'
--     * Possible cause: `length' is applied to too many arguments
--       In the second argument of `on', namely `length []'
--       In the expression: (==) `on` length []

-- :t (==) `on` length @[]
-- (==) `on` length @[] :: [a] -> [a] -> Bool

-- important!! how to use Eval Bot with backticks!
-- `` :t (==) `on` length @[]``


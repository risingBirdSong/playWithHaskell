
import Data.Char 
import Data.List
import Data.Function (on)

-- first wanted to learn about using on since several solutions use it
-- sorts based on strings
cSort_a x tups= take x $ sortBy (compare `on` fst) tups
-- sorts based on numbers
cSort_b x tups= take x $ sortBy (compare `on` snd) tups
mytups = [("cav", 4), ("jas", 5), ("opa", 2), ("abc", 6), ("bac", 16)]


takeMoreTest = take 8 [1,2,3,4] -- [1,2,3,4] 

-- konsumlamm had a nice simple example of `on` that wasnt using sortBy
onshow = ((++) `on` show) 1 2

-- doesnt pass all the newer tests, it was written before them 
a_anagramsFor :: String -> [String] -> [String]
a_anagramsFor word = filter isAnagram
  where
    word' = sort $ map toLower word
    isAnagram = (== word') . sort . map toLower

a_remake :: String -> [String] -> [String]
a_remake word = filter isAnagram
  where 
    word' = sort $ map toLower word
    isAnagram = (== word') . sort . map toLower

-- definition for on  
-- on b u x y runs the binary function b on the results of applying unary function u to two arguments x and y. From the opposite perspective, it transforms two inputs and combines the outputs.

-- from ednob
-- Uh, a_isGramonymOf is equal to \x -> (==) (sort x)
-- isGramonymOf is equal to \x y -> sort x == sort y

-- working
isGramonymOf :: String -> String -> Bool
isGramonymOf = (==) `on` (sort)

-- sort here is the unary function, but `on` calls sort on both lists and then compares 


-- broken
a_isGramonymOf :: String -> String -> Bool
a_isGramonymOf = (==) . sort
-- sort here is unary so it only sorts one list and then compares, which resulted in the broken call
-- here is a working call without on 
withoutOn = (==) "abc" $ sort "bca"


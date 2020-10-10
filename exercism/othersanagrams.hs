
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


-- cool!
addOneOn = (+) `on` (+1)
-- addOn 1 1 -> 4
on_ = (++) `on` (map (+10))

-- another example with two number lists
-- on_ [1,2,3] [101, 102, 103] ->[11,12,13,111,112,113]

numArrArr = [[1,2,3] , [4,5,6],  [7,8,9]]

manyAdder = concat . map (map (+10))  
a_isAnagramOf x y = x /= y && (isGramonymOf x y)
-- a_isAnagramOf "abc" "abc" -> False
-- a_isAnagramOf "abc" "cba" -> True
-- still struggling to understand about that dot , without it filter is applied to few arguments.
-- my intuition is that the dot with the function is being applied with the first arg, and then filtering
-- over the remaining [Chars] remaining in list (2nd argument)
anagramsFor''= filter .  (a_isAnagramOf `on` (map toLower)) 

pa_ana = anagramsFor'' "abc"
-- pa_ana ["cab", "abc"] -> ["cab"]

-- it still seems strange and magical, but comparing the type signature of dot with anagramsFor''
-- I can see that the dot is partially applying (a_isAnagramOf `on` (map toLower)) with the first 
-- string they call the function with, which becomes part of filters predicate func and then runs the list of 
-- words to be filtered. its really cool! 

dot    :: (b -> c) -> (a -> b) -> a -> c
dot f g = \x -> f (g x)

-- destructedAna  = . (a_isAnagramOf `on` (map toLower))

-- compare the type signature with mycompsed, whatever number we apply, it'll become the a type and the x value which will apply to g value resulting in a b type.  which will then be applied to f and result in a c type.

mycomposed = (+5) `dot` (*2) 
composed  = (+5) . (*2) 

c_anagramsFor :: String -> [String] -> [String]
c_anagramsFor = filter . on (==) sortedLetters
  where sortedLetters = sort . map toLower



desc_a = [10,9..1]
dsc x = [x,x-1..1]


-- priming = (\x -> filter (\y -> x (//) y (/=) (\ -> [y,y-1, 1]) ) )
-- priming = (\x -> filter (\y -> x (//) y (/= [y,y-1, 1])) )
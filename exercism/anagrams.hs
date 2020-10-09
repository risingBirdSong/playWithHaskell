import Data.List
import Data.Char
-- Given "listen" and a list of candid

myString :: String
myString = "hello im a friendly string"

myStrings :: [String]
myStrings = ["hello", "im a list of string"]

-- sortedString :: String
sortedString s = sort s

checkAnagram [] [] = True 
checkAnagram _ [] = False 
checkAnagram [] _ = False 
checkAnagram (x:xs) (y:ys) 
  | x == y = checkAnagram xs ys
  | otherwise = False
   -- "    adeefghiiilllmnnorrsty" may want to trim this

sortedAna x y = checkAnagram (sort x) (sort $ map toLower y)

unsortedAna x y = (checkAnagram x  (map toLower y))

anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] =[] 
anagramsFor x (y:ys) 
  | unsortedAna (map toLower x) y == True = anagramsFor x ys
  | sortedAna (map toLower x) y == True = y : anagramsFor x ys
  | otherwise = anagramsFor x ys 

removeSameWords = filter (unsortedAna "banana") ["BANANA", "Banana", "banana"] -- []
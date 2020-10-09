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

sortThenAna :: Ord a => [a] -> [a] -> Bool
sortThenAna x y = checkAnagram x (sort y)

anagramsFor :: String -> [String] -> [String]
anagramsFor x = filter (sortThenAna $ sort $ map toLower x)

masters = anagramsFor "master" ["stream", "pigeon", "maters"]

orchestra = anagramsFor "orchestra" ["cashregister", "carthorse", "radishes"]
-- orchestra = anagramsFor "Orchestra" ["cashregister", "carthorse", "radishes"]


lowered = map toLower 

-- sorteredAndLowered :: [Char] -> [Char]
sorteredAndLowered x = sort $ map toLower x
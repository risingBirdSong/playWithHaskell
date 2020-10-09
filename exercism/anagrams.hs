import Data.List
-- Given "listen" and a list of candid

myString :: String
myString = "hello im a friendly string"

myStrings :: [String]
myStrings = ["hello", "im a list of string"]

-- sortedString :: String
sortedString s = sort s

checkAnagram :: String -> String -> Bool
checkAnagram [] [] = True 
checkAnagram _ [] = False 
checkAnagram [] _ = False 
checkAnagram (x:xs) (y:ys) 
  | x == y = checkAnagram xs ys
  | otherwise = False
   -- "    adeefghiiilllmnnorrsty" may want to trim this

sortThenAna :: String -> [Char] -> Bool
sortThenAna x y = checkAnagram x (sort y)

anagramsFor :: String -> [String] -> [String]
anagramsFor x = filter (sortThenAna $ sort x)

masters = anagramsFor "master" ["stream", "pigeon", "maters"]
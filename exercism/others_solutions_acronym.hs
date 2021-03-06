module Acronym (abbreviate) where
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char


-- base case, if empty, then return empty.
-- case of letter is a letter then prepend it to list, and recurse with tail
-- otherwise prepend an empty string and recurse with tail. this will preserve the originals spacing structure
onlyLetters :: String -> String
onlyLetters "" = ""
onlyLetters (x:xs) | isLetter x || x=='\39' = x:(onlyLetters xs)
                   | otherwise  = ' ':(onlyLetters xs)

-- if character isUpper, prepend an empty space before it, then prepend it, otherwise just prepend it.
-- im thinking this must be to address the problem i ran into where uppercase letters were erased when i splitting on them
separateUpper :: String -> String
separateUpper "" = ""
separateUpper xs | all isUpper xs = xs
separateUpper (x : xs) | isUpper x = ' ':x:(separateUpper xs)
                       | otherwise = x:(separateUpper xs)


-- nice, words is a useful function because it splits a string on any amount of white space to make a list of words
words_a = words "aaa BBB ccc" 
--["aaa","BBB","ccc"]
words_b = words "aaa       BBB                         ccc"
-- ["aaa","BBB","ccc"]
step_a :: String -> [String]
step_a s = words (onlyLetters s)
-- look, for step_b we map because the data at this point in the chain is in the form of [String]
-- step_b shows the usefulness of separateUpper, adding white space right before an Upper lets words effectively split on Upper case which I found to be difficult and turned to the communities inclusiveSplit for a solution. this is much simpler. 
step_b :: [String] -> [[String]]
step_b ss = map (words . separateUpper) ss
step_c :: Foldable t => t [a] -> [a]
step_c ss = concat ss
wordList :: String -> [String]
wordList s = concat (map (words . separateUpper) (words (onlyLetters s)))

-- firstEachLetter takes the head of each substring, capitalizes it, prepends and recurses on the tail. note its input is the shape of the output of wordList
firstEachLetter :: [String] -> String
firstEachLetter [] = ""
firstEachLetter (x:xs) = (toUpper (head x)):(firstEachLetter xs)

-- abbreviate is the caller function that pipes wordList xs to firstEachLetter
abbreviate :: String -> String
abbreviate xs = firstEachLetter (wordList xs)


-- davidjoeressen's solution

abbreviate' :: Text -> Text
abbreviate' = T.concat . map (initial . T.filter isLetter) . T.words . T.replace "-" " "
-- brilliant little helper function ... if the whole substring is uppercase, then just take the first letter
-- otherwise take the first letter, capitalize it, and mappend it to every next uppercase letter if encounters
initial :: Text -> Text
initial "" = ""
initial w
  | T.all isUpper w = T.take 1 w
  | otherwise = (T.toUpper $ T.take 1 w)<>(T.filter isUpper $ T.drop 1 w)


  -- kfish610 brilliant answer
  -- :set  -XViewPatterns    in GHCI to enable this code
firstAndUpper :: String -> String
firstAndUpper ((toUpper->x) : xs) =
    if all isUpper xs then [x] else x : filter isUpper xs
firstAndUpper [] = []

abbreviate'' :: String -> String
abbreviate'' = (>>= firstAndUpper) . words 
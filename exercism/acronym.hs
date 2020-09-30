{-# LANGUAGE OverloadedStrings #-}
import Data.Text   (split) 
import Data.Text as T

import Data.Char


testing = split (=='a') "aabbaca"

-- and pointing out that the docs I was using were old. With the new docs I resolved the error. 

-- note, i began getting this error but this stack overflow fixed the problem

-- https://stackoverflow.com/questions/37894987/couldnt-match-expected-type-text-with-actual-type-char

-- Couldn't match expected type `Text' with actual type `[Char]'

-- fixed with GHCI
-- :set -XOverloadedStrings

-- https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings#:~:text=Haskell%20uses%205%20different%20types%20for%20representing%20strings.&text=The%20String%20type%20is%20a,representation%20most%20suited%20to%20serialization.
-- OverloadedStrings compiler can make your life easier. It allows you to use a string literal refer to any of your different string types.

message = pack "I am not angry. Not at all."
tMap = T.map (\c -> if c == '.' then '!' else c) message
-- mapTest = map head ["apple", "banana", "charlie"] -- "abc" , working test
abbreviate' xs =  Prelude.map T.head . split (==' ') $ xs

mapToUpper = Prelude.map  T.toUpper  $  ["apple", "banana", "charlie"] -- "abc" , working test
mapToHead = Prelude.map  T.head  $  ["apple", "banana", "charlie"] -- "abc" , working test
mapBoth =  Prelude.map  T.head . Prelude.map T.toUpper  $  ["apple", "banana", "charlie"] -- "abc" , working test
 

isUpperSplit xs = split isUpper xs -- results in ["","amel","ase","est"]
-- which is progress, but how to split while including the split character?, like ["","Camel","Case","Test"]

-- thanks to Riuga for this
-- inclusiveSplit p t
--   | T.null t = []
--   | otherwise = 
--       let pre  = fst (T.span p t)
--           post = T.break p (T.drop (T.length pre) t)
--        in (pre <> fst post) : inclusiveSplit p (snd post)

-- $> inclusiveSplit isUpper $ T.pack "CamelCaseTest"
-- ["Camel","Case","Test"]

inclusiveSplit p t
  | T.null t = []
  | otherwise = 
      let pre  = fst (T.span p t)
          post = T.break p (T.drop (T.length pre) t)
       in (pre <> fst post) : inclusiveSplit p (snd post)

incluseTest = inclusiveSplit isUpper $ T.pack "CamelCaseTest"
-- ["Camel","Case","Test"]

packed :: String -> Text
packed x = T.pack x
spanning :: (Char -> Bool) -> Text -> (Text, Text)
spanning p t = T.span p t 
-- O(n) span, applied to a predicate p and text t, returns a pair whose first element is the longest prefix (possibly empty) of t of elements that satisfy p, and whose second is the remainder of the list.
-- T.span :: (Char -> Bool) -> Text -> (Text, Text)
-- so spanning isUpper with a packed text will return empty if first char is lower or as many straight uppers is first is upp
spanningTest str = T.span isUpper . T.pack $ str
-- spanningTest "aaaBBB" -> ("","aaaBBB")
-- spanningTest  "AAAbbb" -> ("AAA","bbb")
-- grab the first of the tuple
spanFstUpper str = fst . T.span isUpper . T.pack $ str
-- spanFstUpper "aaa bbb" -> ""
-- spanFstUpper "AAAbbb" -> ""
-- spanFstUpper "AAABbbcc" -> "AAAB"

-- similar as span but breaks when the predicate fails
breakTest str =  T.break isUpper . T.pack $ str 
-- breakTest "aaa bbb ccc DDD" -> ("aaa bbb ccc ","DDD")
-- breakTest "aaaBBBcccDDD" -> ("aaa","BBBcccDDD")

-- post = T.break p (T.drop (T.length pre) t)
postTest lngth str = T.break isUpper (T.drop lngth . T.pack $ str ) 

-- *Main> postTest 3 "aaabbbccc"
-- ("bbbccc","")
-- *Main> postTest 1 "aaabbbccc"
-- ("aabbbccc","")
-- *Main> postTest 2 "aaabbbccc"
-- ("abbbccc","")
-- *Main> postTest 4 "aaabbbccc"
-- ("bbccc","")

-- Lambda Calculus
-- https://www.youtube.com/watch?v=3VQ382QG-y4&ab_channel=FullstackAcademy
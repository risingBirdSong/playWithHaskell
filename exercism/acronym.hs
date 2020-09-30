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
abbreviate xs =  Prelude.map T.head . split (==' ') $ xs

mapToUpper = Prelude.map  T.toUpper  $  ["apple", "banana", "charlie"] -- "abc" , working test
mapToHead = Prelude.map  T.head  $  ["apple", "banana", "charlie"] -- "abc" , working test
mapBoth =  Prelude.map  T.head . Prelude.map T.toUpper  $  ["apple", "banana", "charlie"] -- "abc" , working test

isUpperSplit xs = split isUpper xs -- results in ["","amel","ase","est"]
-- which is progress, but how to split while including the split character?, like ["","Camel","Case","Test"]

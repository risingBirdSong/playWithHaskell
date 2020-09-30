{-# LANGUAGE OverloadedStrings #-}
import Data.Text   (Text, split) 

testing = split (=='a') "aabbaca"

-- and pointing out that the docs I was using were old. With the new docs I resolved the error. 

note, i began getting this error but this stack overflow fixed the problem

-- https://stackoverflow.com/questions/37894987/couldnt-match-expected-type-text-with-actual-type-char

-- Couldn't match expected type `Text' with actual type `[Char]'

-- fixed with GHCI
-- :set -XOverloadedStrings

-- https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings#:~:text=Haskell%20uses%205%20different%20types%20for%20representing%20strings.&text=The%20String%20type%20is%20a,representation%20most%20suited%20to%20serialization.
-- OverloadedStrings compiler can make your life easier. It allows you to use a string literal refer to any of your different string types.
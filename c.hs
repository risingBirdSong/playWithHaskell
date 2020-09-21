import Data.List

a_ = intersperse 1 [10,20,30,40,50]

works = intersperse '.' "hello world"
-- doesnt = intersperse "." "hello world"
-- why does this break? suspsecting the answer is in thie article
-- https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings

-- while that article is helpful, it doesnt explain the why to my answer however looking at the types does

-- important difference
-- :t "a" -> [Char]
-- :t 'a' -> Char
-- intersperse :: a -> [a] -> [a]

-- notice intersperse takesa singular value and returns a list of those values only 'a' satisfies that constraint


--hurray!
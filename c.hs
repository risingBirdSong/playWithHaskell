-- QQQ question
import Data.List

a_ = intersperse 1 [10, 20, 30, 40, 50]

works = intersperse '.' "hello world"

-- aah look at the type defintion for intercalate
-- intercalate xs xss is equivalent to ( \concat` ( `intersperse` xs xss)) thats what i was looking for
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


calated = intercalate " and " ["abc", "def"]
equivalent = ( concat ( intersperse " test " ["abc", "def"]))

transposed =  transpose [[1,2,3],[4,5,6],[7,8,9]] 
-- ah thats a neat and helpful built in

summed = sum [1,4,8,10]

test = concat [[3,4,5],[2,3,4],[2,1,1]] 
-- cool but doesnt work for any amount of nesting QQQ how?
-- concatted = concat [[1,2,3,4], [1, [4,5], [1, [2,3]]]]

-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat.
replicated = replicate 4 [1,2,3]
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3]]

cAndM = concatMap (replicate 5) [1..5]
-- [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5]
-- whereas i was more expecting [1,2,3,4,5,1,2,3,4,5 etc] 

andtest = and ( map (>4) [5,6,7,8] ) 
andtest1 = and $ map (==4) [5,6,7,8]

-- ok very good but I dont have the intuition for why $ is necessary..
-- ah right, remember the $ operator is for avoiding parenthesis . Anything appearing after it will take precedence over anything that comes before.
-- https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign

orTest1 = or $ map (>4) [1,2,3,4,5] --true
orTest2 = or $ map (>3) [1,2,3] -- false

-- any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively. Usually we use these two functions instead of mapping over a list and then doing and or or.


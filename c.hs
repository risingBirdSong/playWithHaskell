-- QQQ question
import Data.List
import Data.Char
import Data.Function

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

spacesplit = groupBy ((==) `on` isSpace) "hello i am a test"
groupOnCase = groupBy ((==) `on` isUpper) "helloUPPERlowerTESTcase"
isNumberSplit = groupBy ((==) `on` isNumber) "13nd73hfj48j"

splitAndFilterSpaces = (filter (not . any isSpace) . groupBy ((==) `on` isSpace)) "hello i am a test"

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key [] = Nothing  
findKey key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey key xs  

phoneBook =   
  [("betty","555-2938")  
  ,("bonnie","452-2928")  
  ,("patsy","493-2928")  
  ,("lucille","205-2928")  
  ,("wendy","939-8282")  
  ,("penny","853-2492")  
  ] 

bettyNum :: Maybe [Char]
bettyNum = findKey "betty" phoneBook
-- -> Just "555-2938"

-- so Im thinking this is a demonstration of the concept ive heard that once inside a functor a value cannot come out.. notice the above type is a Maybe of [Char] even know we know the value exists, the compiler can't assume that and therefore it's a Maybe... lets try one that doesnt exist.

-- I was wondering why the return value is Just rather than the actual value 555-2938 and I think this wouldnt work because we cant get values out, only transform them, and in this case im wondering if the transformation is from functor Maybe to functor Just

doesntExist :: Maybe [Char]
doesntExist = findKey "not" phoneBook
-- -> Nothing


-- that is interesting that the exact same logic can be interested with fold and the type is the same
findKey_fold :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey_fold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing  

-- ah and Nothing is the accumulator here
-- reminder about the type of Foldable here 
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- fully applying the type
-- String -> [(String, Int)] -> Maybe Int
-- ​
-- ((String, Int) -> Maybe Int -> Maybe Int) -> Maybe Int -> [(String, Int)] -> Maybe Int

-- important!!
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications
-- visible type application

-- to infer the type with a more specific type, call like this
-- :t findKey_fold @Char

-- phoneBook :: [([Char], [Char])]

-- in ghci enable TypeApplications with :set -XTypeApplications, then
  --  :t foldr @[] @(String, Int) @(Maybe Int)
typeApplicationFoldr = foldr @[] @(String, Int) @(Maybe Int) :: ((String, Int) -> Maybe Int -> Maybe Int) -> Maybe Int -> [(String, Int)] -> Maybe Int

-- which you can see satisfies the original generic type!
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- and yes my original thought was correct, but it's certainly useful to prove it and the @ typeApplication is very useful!


-- looking more carefully at using :t foldr with type application
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- matches with     t        a             b
--          foldr @[] @(String, Int) @(Maybe Int)

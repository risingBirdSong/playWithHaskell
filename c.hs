-- QQQ question
import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as Map  


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
-- typeApplicationFoldr = foldr @[] @(String, Int) @(Maybe Int) :: ((String, Int) -> Maybe Int -> Maybe Int) -> Maybe Int -> [(String, Int)] -> Maybe Int

-- which you can see satisfies the original generic type!
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- and yes my original thought was correct, but it's certainly useful to prove it and the @ typeApplication is very useful!


-- looking more carefully at using :t foldr with type application
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- matches with     t        a             b
--          foldr @[] @(String, Int) @(Maybe Int)
mapFromList :: Map.Map [Char] [Char]
mapFromList = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 

-- If there are duplicate keys in the original association list, the duplicates are just discarded. This is the type signature of fromList

-- signature for Map.fromList
-- Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v  

-- You should always use Data.Map for key-value associations unless you have keys that aren't part of the Ord typeclass.

mapA = Map.insert 5 25 Map.empty
mapB = Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))  
mapBB = Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty  

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty  
-- notice that this will fail without the type
mapStr = Map.insert "hello" "world" Map.empty

-- lookup works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key and Nothing if it doesn't.

-- stuck on errors while trying to lookup keys from the maps using lookup
-- lookup "hello" mapStr
-- forgetting that lookup belongs to the Map import
-- Map.lookup "hello" mapStr
-- works

-- very nice
-- fromListWith is a cool little function. It acts like fromList, only it doesn't discard duplicate keys but it uses a function supplied to it to decide what to do with them. Let's say that a girl can have several numbers and we have an association list set up like this.

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  
mycar = Car {company="Ford", model="Mustang", year=1967}  
-- broke = Car {company="Ford", model="Mustang"} i was wonderin, and it does, throw an exception, all fields are mandatory

-- Vector :: a -> a -> a -> Vector a
data Vector a = Vector a a a deriving (Show)  

-- so here the t type parameter represents three numbers as arguments since that's what is required by type Vector?
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n) 

vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  

-- side quest


-- tried to get up and running with exercism and ran into a whole new set problems
--learned some more about cabal but also found many errors and roadblocks
-- helpful resources 
-- https://www.youtube.com/watch?v=a7R-2vtPLDM&ab_channel=HaskellatWork

-- which teaches that to download a package, we add the package to the project.cabal file, like 

-- build-depends:       base >=4.14 && <4.15
--                      , random

-- The imports come after the module declaration

-- btw, if you have cabal 3+ you don't need to worry about the new- prefix on commands

-- Yep, so you can either import specific functions / types (or all) with import System.Random (uniformR). Or you can import everything but have it namespaced import qualified System.Random

-- For the latter, you would use it as System.Random.uniformR

-- you can further rename the qualified namespace like import qualified System.Random as Rand

-- to get an external package working in ghci this worked ... 
-- cabal repl from powershell terminal

-- back to regular lessons

type MyString = [Char]  

-- type PhoneBook = [(String,String)]  
type PhoneNumber = String  
type Name = MyString  
type PhoneBook = [(Name,PhoneNumber)]  

phoneBook :: PhoneBook
mypb = [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]  

    -- thats a really nice syntax using these syntaxes
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook  


-- an added resource for how to do imports in haskell
-- https://wiki.haskell.org/Import

-- import qualified Data.Map as Map  
  
-- data LockerState = Taken | Free deriving (Show, Eq)  
  
-- type Code = String  
  
-- type LockerMap = Map.Map Int (LockerState, Code) 

-- for exercism this bug report suggestion worked for me! Building with cabal instead of stack
-- https://github.com/haskell/haskell-ide-engine/issues/1779

addTen :: Num a => a -> a
addTen = (+ 10)
-- addFifteen :: Integer -> Integer
-- why the difference in inferred types between ten and 15?
addFifteen :: Num a => a -> a
addFifteen = (+ 15) 

-- and why the differnet signatures between adding and multing?

-- multThenAdd :: (Num a) => (a -> b) -> (b -> c) -> (a -> c)
adding = addTen . addFifteen . addTen . addFifteen

--force chage


-- ok interesting syntax
desugaredList = 3:4:5:6:[]

data List a = Empty | Cons (List a) deriving (Show, Read, Eq, Ord) 
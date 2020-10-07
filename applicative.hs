import Control.Applicative
import Control.Monad
import Data.Semigroup
import qualified Data.Foldable as F  
{-# LANGUAGE TypeFamilies #-}

    -- because 4 are not conained, not in functors
-- doesntWork = (+) <$> 4 <*> 4
-- this does work because both vals are in functors
applicative_a = (+) <$> [4] <*>[4] -- [8]
applicative_b = (+) <$> Just 5 <*> Just 5 -- Just 10
-- notice how when you a val and function inside of just, that basically doing the job of <$>
applicative_c = Just (5+) <*> Just 5 -- Just 10

ziplista =  getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  

-- zipWith takes a function that takes two parameters and zips two lists with it. zipWith3 takes a function that takes three parameters and zips three lists with it, and so on
zippedWith =  zipWith (+) [1,2] [3,4]
applicativeStyle = (:) <$> Just 3 <*> Just [4] 
lifted = liftA2 (:) (Just 3)(Just [4])

-- so it looks like lift is a syntactic sugar for the applicative style

sequenceA' :: (Applicative f) => [f a] -> f [a]  
sequenceA' [] = pure []  
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs 

-- ah and I thought i was overly hung up on sequenceA' but really its just doing a familiar pattern like in applicativeStyle = (:) <$> Just 3 <*> Just [4] but just abstracting it into recursive pattern.

-- sequenceA [Just 4, Just 5]
-- Just [4,5]

--I think the most common way to think about it is combining the inner effects into a single outer effect." -Arc

sequenceA_ :: (Applicative f) => [f a] -> f [a]  
sequenceA_ = foldr (liftA2 (:)) (pure [])  

-- and what are use cases of this sort of behavior?
-- ghci> sequenceA [[1,2,3],[4,5,6]]  
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

-- allcombos = sequence (replicate 4 ['a'..'z'])
takeOnlySome = take 52 $ sequence (replicate 4 ['a'..'z'])
--monadic
cleaner = take 20 $ replicateM 4 ['a'..'z']
-- ["aaaa","aaab","aaac","aaad","aaae","aaaf","aaag","aaah","aaai","aaaj","aaak","aaal","aaam","aaan","aaao","aaap","aaaq","aaar","aaas","aaat","aaau","aaav","aaaw","aaax","aaay","aaaz","aaba","aabb","aabc","aabd","aabe","aabf","aabg","aabh","aabi","aabj","aabk","aabl","aabm","aabn","aabo","aabp","aabq","aabr","aabs","aabt","aabu","aabv","aabw","aabx","aaby","aabz"]

-- sequenceA_ [(+3), (+10), (+20)] 5
-- [8,15,25]

-- sequenceA [(>4),(<10),odd] 7  
-- [True,True,True]  

subsequences xs = filterM (const [True,False]) xs
filterAllCombos = filterM (const [True,False]) [1..4]
-- [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]

-- you can think of a List as a nondeterministic choice of one of the elements
-- and the list monad models this
-- when you do do x <- [1,2,3]; y <- [1,2,3]; pure (x,y) you can read this as "nondeterministically choose an element from the first list and an element from the second list and return a pair of the elements chosen
-- and you get back a "nondeterministic" results, i.e. a list of all possible pairs
-- filterM lets you filter a list in a monadic context
-- for example you can ask the user whether to keep each element in the list or not
-- filterM (\x -> print x >> readLn) [1,2,3]
-- this is in the IO monad
-- the user can type True or False to decide whether the element should be kept in the result
-- so in the List monad we can just ignore the element and return [True,False], i.e. a nondeterministic choice between keeping it in the result or not
-- and we get a "nondeterministic" result, that is a list of all such possible filtered lists


gottenList = getZipList $ ZipList [1,2,3] -- [1,2,3]
zipFuncsWithVals = getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3] -- [2,200,15]

-- and remember the simple way to batch lists ->
simpleBatch = [1,2,3] ++ [4,5,6] -- [1,2,3,4,5,6]


data Profession = Fighter | Archer | Accountant  deriving (Show, Eq, Read)
  
data Race = Human | Elf | Orc | Goblin  deriving (Show, Eq, Read)
  
data PlayerCharacter = PlayerCharacter Race Profession  deriving (Show, Eq, Read)

-- PlayerCharacter :: Race -> Profession -> PlayerCharacter
myHuman = Human 
myFighter = Fighter
myHumanFighter = PlayerCharacter Human Fighter
myElfArcher = PlayerCharacter Elf Archer
myGoblinAccountant = PlayerCharacter Goblin Accountant

threeTupleLengthTest = (1,2,3)
fourTupleLengthTest = (1,2,3,4)

lazinessInAction = head [3,4,5,undefined,2,undefined] 
-- notice it compiles but will be an error if run 
lazinessInAction_error = head [undefined,4,5,undefined,2,undefined]  

-- A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function. When something acts as an identity with respect to a function, it means that when called with that function and some other value, the result is always equal to that other value

-- cool i've used mconcat before but didnt know the m stood for monoid, it makes sense now
monoidConcat = mconcat [[1,2],[3,6],[9]] -- [1,2,3,6,9]

instance Semigroup Any' where
    (<>) = mappend

newtype Any' = Any' { getAny' :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded) 

instance Monoid Any' where  
    mempty = Any' False  
    Any' x `mappend` Any' y = Any' (x || y) 

testCustomAny = getAny' $ Any' True `mappend` Any' False -- True

-- For all intents are purposes there is only one operation in Haskell: applying a pure function to another value. -Arc
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )
testingTree_add = F.foldl (+) 0 testTree  -- 42
testingTree_mult = F.foldl (*) 1 testTree -- 64800

applicativeMax = max <$> Just 3 <*> Just 6  
applicativeMaxMany = fmap max [Just 4, Just 12, Just 5]

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x  

--Just 10 `applyMaybe` (\x -> Just (x + 5)) -- Just 15
-- Nothing `applyMaybe` (\x -> Just (x + 5))  Nothing

-- Just "smile" `applyMaybe` \x -> Just (x ++ " :)")  Just "smile :)"  

-- Ah cool, I just out of the blue thought to put the type here and it works! Previously for example, I ran pure 5 and just got back 5, no structure, putting the type gives it the structure!
-- pure 4 :: [Int]
-- [4]
-- same with Monad version with return 
-- return 5 :: [Int]
-- [5]
-- but why not with Maybe?
-- pure 5 :: Maybe 
-- * Expecting one more argument to `Maybe'
--       Expected a type, but `Maybe' has kind `* -> *'

-- PrettyPrincessKitty FS thanks
-- it works like ->
--   pure 5 :: Maybe Int
-- Just 5

returned = return "WHAT" :: Maybe String -- Just "WHAT"
bound y = y >>= (\x -> Just (x * 10)) -- Just 90 
bounda y = y >>= (\x -> return (x * 10))
-- bounda (Just 7)
-- Just 70



myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs 

-- had a question about immutability and ednob gave a good example of immutability in action using simple map
-- myMap (\x -> x+1) [1,2,3]
-- myMap (\x -> x+1) (1:2:3:[])
-- (1 + 1) : myMap (\x -> x + 1) (2:3:[])
-- (1 + 1) : (2 + 1) : myMap (\x -> x + 1) (3:[])
-- (1 + 1) : (2 + 1) : (3 + 1) : myMap (\x -> x + 1) []
-- (1 + 1) : (2 + 1) : (3 + 1) : []
type Birds = Int  
type Pole = (Birds,Birds)  

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing 


    -- landLeft 2 (0,0)   Just (2,0)
    -- this is the problem, because the function takes a Pole  returns a maybe Pole, we can't chain like this ->
-- landLeft 1 (landRight 1 (0,0))    
  -- * Couldn't match type `Maybe Pole' with `(Birds, Birds)'
  -- Expected type: Pole
  --   Actual type: Maybe Pole
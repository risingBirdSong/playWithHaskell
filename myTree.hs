data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
data Tree' a = Empytness | Node' a (Tree' a) (Tree' a) deriving (Show, Read, Eq)  

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

oneTree = singleton 5

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 

treeAddLeft = treeInsert 4 oneTree
treeAddRight = treeInsert 6 oneTree

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 

treeElem' :: (Ord a) => a -> Tree a -> Bool  
treeElem' x EmptyTree = False  
treeElem' x (Node a left' right')  
    | x == a = True  
    | x < a  = treeElem x left'
    | x > a  = treeElem x right' 

-- ok interesting so here is immutability in action, log out the three trees 
-- oneTree Node 5 EmptyTree EmptyTree

-- *Main> treeAddLeft 
-- Node 5 (Node 4 EmptyTree EmptyTree) EmptyTree

-- *Main> treeAddRight 
-- Node 5 EmptyTree (Node 6 EmptyTree EmptyTree)

-- see they are totally distinct from each other!

nums = [8,6,4,1,7,3,5]  
numsTree = foldr treeInsert EmptyTree nums  
-- (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

-- *Main> treeElem 8 numsTree  
-- True
-- *Main> treeElem 99 numsTree 
-- False
-- infix syntax
-- *Main>  8 `treeElem` numsTree
-- True

-- weird and interest... type definition where each assertion is the negation of the opposite
-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)  

-- great and interesting answer to my previous observation 

-- Because == was defined in terms of /= and vice versa in the class declaration, we only had to overwrite one of them in the instance declaration. That's called the minimal complete definition for the typeclass — the minimum of functions that we have to implement so that our type can behave like the class advertises. To fulfill the minimal complete definition for Eq, we have to overwrite either one of == or /=. If Eq was defined simply like this:

data TrafficLight = Red | Yellow | Green  
data TrafficLight_d = Red' | Yellow' | Green' deriving (Eq, Show)

-- making TrafficLight an instance of the Eq typeclass
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False 

-- which allows
-- *Main> Green == Green
-- True
-- *Main> Green == Red  
-- False
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light" 

-- which allows for
-- *Main> Red
-- Red light

-- ah with reading about functors the following makes much more sense
-- *Main> fmap (+1) (Just 2)
-- Just 3 

-- fmap (Just )( Nothing) 
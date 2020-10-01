data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

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

-- ok interesting so here is immutability in action, log out the three trees 
-- oneTree Node 5 EmptyTree EmptyTree

-- *Main> treeAddLeft 
-- Node 5 (Node 4 EmptyTree EmptyTree) EmptyTree

-- *Main> treeAddRight 
-- Node 5 EmptyTree (Node 6 EmptyTree EmptyTree)

-- see they are totally distinct from each other!
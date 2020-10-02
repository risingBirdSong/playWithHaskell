nums = [1,3, 7,9,11,21,39,76]
findNum :: Eq a => a -> [a] -> Maybe a
findNum _ [] = Nothing
findNum srch (x:xs) 
  | srch == x =  Just srch
  | otherwise = findNum srch xs
-- 
findNumSorted srch (x:xs) 
  | srch == x = Just x
  | x > srch = Nothing
  | otherwise = findNumSorted srch xs


data MyTree a = EmptyTree | Node a (MyTree a) (MyTree a) deriving (Show, Read, Eq)  

singleton x = Node x EmptyTree EmptyTree

insertVal v EmptyTree = singleton v
insertVal v (Node x lefty righty)
  | v == x = Node x lefty righty
  | v < x = Node x (insertVal v lefty) righty
  | v > x = Node x lefty (insertVal v righty)
  
testTree = foldr insertVal EmptyTree [9,1,8,2,7,3,6,4,5]
-- Node 5 (Node 4 (Node 3 (Node 2 (Node 1 EmptyTree EmptyTree) EmptyTree) EmptyTree) EmptyTree) (Node 6 EmptyTree (Node 7 EmptyTree (Node 8 EmptyTree (Node 9 EmptyTree EmptyTree))))

findValue _ EmptyTree = Nothing
findValue srch (Node x left_b right_b) 
  | srch == x = Just x
  | srch < x = findValue srch left_b
  | srch > x = findValue srch right_b

-- findValue 3 testTree Just 3 
-- findValue 9 testTree Just 9 
-- findValue (-100) testTree Nothing 

-- cheeky remake of linkedin javascript function about persistently appyling for jobs I saw that would crash
-- while (true) {keepApplying ++}

keepApplying = replicate 1000000000000 "keep applying"
keepApplyingButDontCrash = take 10 keepApplying  
-- ["keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying"]

-- remake Tree from scratch for practice... I see why i used x and a here... but its not right, remember this is a
-- typeclass, so x is a type parameter, so we can use x throughout, its not saying its the same value, its saying its the same type.

-- Voidy :: Tree' x
-- Nodey :: x -> Tree' x -> Tree' x -> Tree' x
data Tree' x = Voidy | Nodey x (Tree' x) (Tree' x) deriving (Show, Read, Eq)  

-- did it like this at first using Tree' but notive that Tree' is a typeclass, and for an instantiation, we want to use a value, therefore we use the name of the value-level constructor (discussed in more detail below from help of Digi) designated in Tree' that conforms to the contract, in this case Nodey ...
-- single v = Tree' v Voidy Voidy
single v = Nodey v Voidy Voidy

-- all this helpful info from Digi (Discord)

-- "constructor"
-- is what they're called
-- Tree' is not a typeclass, it is a type-level constructor

-- typeclasses are used to constrain polymorphic types, type-level constructors are what they sound like

-- so Tree' defines a type-level constructor and two value-level constructors

-- type-level constructors construct types
-- so like, Tree' is a type constructor
-- it takes in a type and 'holds onto it' in the same way a value-level constructor does with values

insertingToTree v Voidy = single v
insertingToTree v (Nodey x (leftB) (rightB)) 
  | v == x = Nodey x leftB rightB
  | v < x = Nodey x (insertingToTree v leftB) rightB
  | v > x = Nodey x leftB (insertingToTree v rightB)

--   *Main> insertingToTree 4 Voidy
-- Nodey 4 Voidy Voidy

addToTree v Voidy = single v
addToTree v (Nodey x left right)
  | v == x = Nodey x left right
  | v < x = Nodey x (addToTree v left) right
  | v > x = Nodey x left (addToTree v right)
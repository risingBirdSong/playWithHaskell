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

keepApplying = replicate 1000000000000 "keep applying"
keepApplyingButDontCrash = take 10 keepApplying  
-- ["keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying","keep applying"]



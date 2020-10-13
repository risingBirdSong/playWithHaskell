data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Read)

single a = Node a (Empty) (Empty)

insertVal x Empty = single x
insertVal x (Node a l r)
  | x == a = (Node a l r)
  | x > a = Node a (l) (insertVal x r)
  | x < a = Node a (insertVal x l) (r)

testTree = foldr insertVal Empty ([90,10,80,20,70,30,38,35,60,40,49,48,45,50])
imbalanced = foldr insertVal Empty ( [5 ,3,7,1,9])

search_b _ Empty = False
search_b x (Node v l r)
  | x == v = True
  | x < v = (search_b x l)
  | x > v = (search_b x r)

search_v _ Empty = Nothing
search_v x (Node v l r)
  | x == v = Just v
  | x < v = search_v x l
  | x > v = search_v x r

height Empty = -1
height (Node a l r) = 1 + max (height l) (height r)


balanced Empty = True
balanced (Node a l r)
  | not (balanced l) = False
  | not  (balanced r) = False
  | abs ((height l) - (height r)) > 1 = False
  | otherwise = True

indent = map ("  "++)

layoutTree Empty = []  -- wow, that was easy
layoutTree (Node here left right) 
         = indent (layoutTree left) ++  [show here]  ++ indent (layoutTree right)

prettyPrint t = unlines . layoutTree $ t
-- https://stackoverflow.com/questions/19082560/haskell-pretty-print-binary-tree-not-displaying-properly
-- *Main> putStrLn (prettyPrint testTree) 
--             10
--           20
--         30
--       35
--         38
--     40
--   45
--     48
--       49
-- 50
--   60
--     70
--       80
--         90
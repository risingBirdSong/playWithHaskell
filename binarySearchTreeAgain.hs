data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Ord, Eq)

single :: a -> Tree a
single a = Node a (Empty) (Empty)


insertVal :: Ord a => a -> Tree a -> Tree a
insertVal x Empty = single x
insertVal x (Node a l r)
  | x == a = Node x l r
  | x < a = Node x (insertVal x l) r
  | x > a = Node x (l) (insertVal x r)

testTree_r = foldr insertVal Empty [5,3,7,1,9]

search x Empty = False
search x (Node a l r)
  | x == a = True
  |x < a = search x l
  | x > a = search x r
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

single a = Node a (Empty) (Empty)

insertVal x Empty = single x
insertVal x (Node a l r)
  | x == a = (Node a l r)
  | x > a = Node a (l) (insertVal x r)
  | x < a = Node a (insertVal x l) (r)

testTree = foldr insertVal Empty [5 ,3,7,1,9]

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

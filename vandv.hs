
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Read)


single v = Node v (Empty) (Empty)

insertVal :: Ord a => a -> Tree a -> Tree a
insertVal x (Empty) = single x
insertVal x (Node v l r)
  | x == v = Node v l r
  | x < v = Node v (insertVal x l) (r)
  | x > v = Node v (l) (insertVal x r)
  
test = foldr insertVal Empty [70,30,60,40,50]
testa = foldr insertVal Empty ["c","b","a"]

search x (Empty) = Nothing
search x (Node v l r)
    | x == v = Just x
    | x < v = search x l
    | x > v = search x r

data MyTree a = Empty | Node a (MyTree a) (MyTree a) deriving (Show, Eq, Read)

singleton x = Node x (Empty) (Empty)

insertVal v Empty = singleton v 
insertVal v (Node x left_ right_)
  | v == x = Node v left_ right_
  | v < x = Node v (insertVal v left_) right_
  | v > x = Node v left_ (insertVal v right_)


testing__ = foldr insertVal Empty [1,9,5,2,8,3,7]



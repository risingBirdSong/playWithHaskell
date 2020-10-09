-- Introduction
-- Implement the keep and discard operation on collections. Given a collection and a predicate on the collection's elements, keep returns a new collection containing those elements where the predicate is true, while discard returns a new collection containing those elements where the predicate is false.

-- For example, given the collection of numbers:

-- 1, 2, 3, 4, 5
-- And the predicate:

-- is the number even?
-- Then your keep operation should produce:

-- 2, 4
-- While your discard operation should produce:

-- 1, 3, 5
-- Note that the union of keep and discard is all the elements.

-- The functions may be called keep and discard, or they may need different names in order to not clash with existing functions or concepts in your language.

-- my working solution
discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = [] 
discard p (x:xs) 
  | p x == False = x : discard p xs
  | otherwise = discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) 
  | p x == True = x : keep p xs
  | otherwise = keep p xs

-- official solution

filter_o :: (a -> Bool) -> [a] -> [a]
filter_o _pred []    = []
filter_o pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs

  -- communit solutions
-- notice the implicit truthiness / falsiness
discard_a _ [] = []
discard_a p (x : xs) | p x       = discard p xs
                   | otherwise = x : discard p xs
keep_a _ [] = []
keep_a p (x : xs) | p x       = x : keep p xs
                | otherwise = keep p xs

-- notice the clever use of of not  pred for discard. 
discard_b :: (a -> Bool) -> [a] -> [a]
discard_b p = keep (not . p)

keep_b :: (a -> Bool) -> [a] -> [a]
keep_b p xs = [a | a <- xs, p a]

-- twoOrFour = keep (==2 || ==4)

-- foldr (.) id 


a_pipeline v = map (==v) [1,2,3]
-- pipeline 3 -> [False,False,True]

-- was trying things like 
-- ((==2) || (==4))
orEquals x
  | x == 2 = True
  | x == 4 = True
  | otherwise = False

b_pipeline = keep (orEquals) [1,2,3,4,5] -- [2,4]

-- official implementation
even_o n          =  n `rem` 2 == 0
odd_o             =  not . even

-- i wrote 
even' = (\x -> x `mod` 2 == 0 )
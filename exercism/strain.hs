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

strain fn [] = []
strain fn (x:xs) 
  | fn x == True = x : strain fn xs
  | otherwise = strain fn xs

-- twoOrFour = strain (==2 || ==4)

-- foldr (.) id 


a_pipeline v = map (==v) [1,2,3]
-- pipeline 3 -> [False,False,True]

-- was trying things like 
-- ((==2) || (==4))
orEquals x
  | x == 2 = True
  | x == 4 = True
  | otherwise = False

b_pipeline = strain (orEquals) [1,2,3,4,5] -- [2,4]
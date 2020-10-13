myAppA fs xs= [f x | f <- fs , x <- xs]
-- myAppA [(+1)] [1,2,3,4] -> [2,3,4,5]
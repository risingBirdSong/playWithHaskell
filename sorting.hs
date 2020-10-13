quicksort_in :: (Ord a) => [a] -> [a]  
quicksort_in [] = []  
quicksort_in (x:xs) =   
    let smallerSorted = quicksort_in [a | a <- xs, a <= x]  
        biggerSorted = quicksort_in [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
sorted_a =  quicksort_in [9,1,8,2,7,3,6,3,5,4]

quickSort_where [] = []
quickSort_where (x:xs) = smallerSorted ++ [x] ++ biggerSorted
  where
    smallerSorted = quickSort_where [a | a <- xs, a <= x]
    biggerSorted = quickSort_where [a | a <- xs, a > x]
sorted_b = quickSort_where [9,1,8,2,7,3,6,4,5]

expr = let x = 1 in 2 -- 2
--I read let x = 1 in foo x as "let/allow  x to be 1 when evaluating the expression foo x"
-- So the in is kind of what the function is itself, while the let part of the body is sort of the helper for building the in part?


bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:arrLeft) = bubbleSort(init bubbled) ++ [last bubbled]
    where (smaller,bigger) = if(x <= y) then (x, y) else (y, x)
          bubbled = [smaller] ++ bubbleSort (bigger:arrLeft)
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
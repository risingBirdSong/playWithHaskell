nums = [1,3, 7,9,11,21,39,76]
findNum :: Eq a => a -> [a] -> Maybe a
findNum _ [] = Nothing
findNum srch (x:xs) 
  | srch == x =  Just srch
  | otherwise = findNum srch xs


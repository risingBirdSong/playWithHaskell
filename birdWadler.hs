square :: (Num a) => a -> a
square x = x * x

quad :: (Num a) => a -> a
quad x = square $ square x

mymax :: (Ord a) => a -> a -> a
mymax x y 
  | x > y = x
  | y > x = y
  | otherwise = x

circleArea :: Fractional a => a -> a
circleArea r = (22 / 7) * (square r)
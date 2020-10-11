primes :: [Integer]
primes = 2 : map head (iterate (\(x:xs) -> filter ((/= 0) . (`mod` x)) xs) [3,5..])

nth :: Int -> Integer
nth n = primes !! (n - 1)
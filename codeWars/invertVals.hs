invert :: (Num a) => [a] -> [a]
invert = map (*(-1))

-- others solutions
invert' :: [Integer] -> [Integer]
invert' = map negate
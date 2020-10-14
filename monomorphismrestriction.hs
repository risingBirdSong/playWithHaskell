
-- A simple example is plus = (+). Without an explicit signature for plus, 
-- the compiler will not infer the type (+) :: (Num a) => a -> a -> a for `plus`, 
-- but will apply defaulting rules to specify plus :: Integer -> Integer -> Integer. 

-- and yes sure enough 
plus = (+)

-- here the inferred type doesnt work as expected but adding the more generic type does
typedPlus :: (Num a) => a -> a -> a
typedPlus = (+)

-- To a first approximation it means that you often cannot overload a function 
-- unless you provide an explicit type signature.
-- https://wiki.haskell.org/Monomorphism_restriction
-- A simple example is plus = (+). Without an explicit signature for plus, 
-- the compiler will not infer the type (+) :: (Num a) => a -> a -> a for `plus`, 
-- but will apply defaulting rules to specify plus :: Integer -> Integer -> Integer. 

-- and yes sure enough 
import Data.List
plus = (+)

-- here the inferred type doesnt work as expected but adding the more generic type does
typedPlus :: (Num a) => a -> a -> a
typedPlus = (+)

-- To a first approximation it means that you often cannot overload a function 
-- unless you provide an explicit type signature.

f xs = (len,len)
     where
       len = genericLength xs

-- The type of f, if no signature is given, then the compiler doesn't know that the two elements of the returned
-- pair are of the same type. It's return value will be:
-- f::(Num a, Num b) => [x] -> (a, b)

-- why not
-- Num a => [x] -> (a, a)

-- the article then goes into more details and examples that are currently difficult for me to follow...
-- So for now the takeaway is that it's very handy to type your own signatures and if an inferred type 
-- seems off, then monomorphism restriction is a good thing to consider as the problem
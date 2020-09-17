-- tellMeType = :t "t"

--  :: is like saying type of

-- Explicit types are always denoted with the first letter in capital case. 

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

-- i thought maybe -> separating all the inputs implied a currying but it errored when I called with one arg
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

factorial :: (Integral a) => a -> a  
factorial 0 = 1;
factorial n = product [1..n] 

-- :t head
-- head :: [a] -> a

-- what is a?
-- Because it's not in capital case it's actually a type variable.

-- . Functions that have type variables are called polymorphic functions. 

-- :t (==)  
--constraint \/
-- (==) :: Eq a => a -> a -> Bool

-- ghci> :t (>)  
-- (>) :: (Ord a) => a -> a -> Bool  

-- thats a cool and unique feature
-- 5 `compare` 3 -> GT
-- 4 `compare` 4 -> EQ

-- Num is a numeric typeclass. Its members have the property of being able to act like numbers. Let's examine the type of a number.
-- ghci> :t 20  
-- 20 :: (Num t) => t 
-- It appears that whole numbers are also polymorphic constants. They can act like any type that's a member of the Num typeclass.

-- ghci> 20 :: Int  
-- 20  
-- ghci> 20 :: Integer  
-- 20  
-- ghci> 20 :: Float  
-- 20.0  
-- ghci> 20 :: Double  
-- 20.0  


-- ah great, this Num is a great example of typeclass and helps me grasp the concept!


-- wildcard `_' type
firstTuple :: (a, b , c) -> a
firstTuple (x, _ , _ ) = x;

secondTuple (_, b, _) = b;

thirdTuple (_,_,z) = z;

-- well said and important
-- A pattern like x:xs will bind the head of the list to x and the rest of it to xs
-- The x:xs pattern is used a lot, especially with recursive functions.

-- If you want to bind, say, the first three elements to variables and the rest of the list to another variable, you can use something like x:y:z:zs. It will only match against lists that have three elements or more.

-- i tried to return a string rather than error but this doesnt work becuase it infers to be string array...
-- can this be done? but perhaps we want the error? I think so! Because then when our program compiles we can have much more confidence in it
-- head' :: [a] -> a  
-- head' [] = "empty list"  
-- head' (x:_) = x  

header :: [a] -> a
header [] = error "empty list!"
header (x:_) = x
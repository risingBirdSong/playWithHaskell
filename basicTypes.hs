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

getLength xs = sum [1 | _ <- xs]

tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "the List has two elements" ++ show x ++ " and " ++ show y
-- thanks Riuga on Discord showed me error, I wasnt using parens() to wrap expression
-- compiler is interpreting -> (show length) xs , which is wrong 
tell (x:xs) = "the list has a length of " ++ show (length xs + 1)



-- showlength xs = show getLength xs

length' [] = 0;
length' (_:xs) = 1 + length' xs

sum_ [] = 0
sum_ (x:xs) = x + sum_(xs)

-- patterns
sayAll "" = "empty string"
sayAll all@(x:xs) = "say " ++ all ++ " single " ++ [x]

guardPractice compare 
    |  compare <= 10 = "less than ten"
    |  compare <= 30 = "between 10 and 30"
    |  compare <= 100 = "between 30 and 100"  
    |  otherwise = "over 100"

max' a b
    | a > b = a
    | otherwise = b

a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | a < b = LT


-- example of let in bindings
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

  
    -- let binding in list comprehension

calcBmis_a xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  

-- If let bindings are so cool, why not use them all the time instead of where bindings, you ask? Well, since let bindings are expressions and are fairly local in their scope, they can't be used across guards. Some people prefer where bindings because the names come after the function they're being used in. That way, the function body is closer to its name and type declaration and to some that's more readable.

maximum' :: Ord a => [a] -> a
maximum' [] = error "error no list"
maximum' [x] = x;
maximum' (x:xs) 
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

  -- how this is working internally

  -- Let's take an example list of numbers and check out how this would work on them: [2,5,1]. If we call maximum' on that, the first two patterns won't match. The third one will and the list is split into 2 and [5,1]. The where clause wants to know the maximum of [5,1], so we follow that route. It matches the third pattern again and [5,1] is split into 5 and [1]. Again, the where clause wants to know the maximum of [1]. Because that's the edge condition, it returns 1. Finally! So going up one step, comparing 5 to the maximum of [1] (which is 1), we obviously get back 5. So now we know that the maximum of [5,1] is 5. We go up one step again where we had 2 and [5,1]. Comparing 2 with the maximum of [5,1], which is 5, we choose 5.

max_a :: Ord a => [a] -> a
max_a [] = error "empty list"
max_a [x] = x
max_a (x:xs) = max x (max_a xs)

-- the type inferred by compiler
replicate_ :: (Ord t, Num t, Num a) => a -> t -> [a]
replicate_ n c 
    | c <= 0 = []
    | otherwise = n:replicate_ (n) (c-1)

    -- interesting insight and explanation
-- We used guards here instead of patterns because we're testing for a boolean condition. If n is less than or equal to 0, return an empty list. Otherwise return a list that has x as the first element and then x replicated n-1 times as the tail. Eventually, the (n-1) part will cause our function to reach the edge condition.


-- Question on this note but why isnt num a sublass of Ord? Because all nums are orderable.
-- Note: Num is not a subclass of Ord. That means that what constitutes for a number doesn't really have to adhere to an ordering. So that's why we have to specify both the Num and Ord class constraints when doing addition or subtraction and also comparison.

-- first attempt at sig
-- take_ :: (Num t, Num [t]) -> [t] 

-- inferred type
                  -- inputs  \/    \/      \/ output
-- take_ :: (Ord t, Num t) => t -> [a] -> [a]
take_ n xs 
    | n <= 0 = []
take_ _ [] = []
take_ n (x:xs) = x:take_ (n-1) xs

-- inferred type
reverse_a :: [a] -> [a]
reverse_a [] = []
reverse_a (x:xs) = reverse_a xs ++ [x]

-- infinite list
repeat' :: a -> [a]  
repeat' x = x:repeat' x  

-- this one is interesting because nothing about repeat' x explicitly suggests a list... but the x: before
-- it, is this what treats it to act like a list?

-- experiment ... a dumb infinite recurse without the x: prepending, lets look at the type.
infiniteloop :: t1 -> t2
infiniteloop x = infiniteloop x 

-- cool, this makes sense, no list type... which means that the x: in repeat is what's responsible for 
-- treating the output as a list. is a there a term for this? casting, coercion, deriving types? 

-- inferred type is a list output, with the only difference we're prepending with x: , which is proof that syntax return the output as list type. 
infiniteloopAsList :: t -> [t]
infiniteloopAsList x = x:infiniteloopAsList x 



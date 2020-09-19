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


zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys 

elem' val [] = False
elem' val (x:xs) 
    | (x == val) = True
    | otherwise = elem' val xs


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 



-- higher order functions

-- example
-- max__ :: Ord a => a -> a -> a

-- All the functions that accepted several parameters so far have been curried functions.

-- and thats why we see a each type separated by ->, like max's a -> a -> a

-- we can see that by supplying fewer arguments to a function
curryTest = max 5
maxResult = curryTest 10 
-- results in 10


-- the parens in the type signature means its a function.
--   a function \/
applyTwice :: (a->a) -> a -> a
applyTwice f x = f(f x)

addFiveTwice = applyTwice(+5)
-- applyTwice (++ " haha ") "hello"
-- "hello haha  haha "


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith'  _ [] _ = []
zipWith'  _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys 

-- two things, my crack at it was appending tuples with the func applied to each, here were just appending 
-- straight list.
-- and silly me, i had the function correct, but was calling it wrong with a bad func call. I was calling it
-- with a func AND val like (+3) but no, we want just func, the val argumens come from the two lists

veryInterestingRecursiveZip = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  

-- wrong associativity, error
-- flip' f = g
--     where f x y = g y x

-- fixed, ensure the right assignment is defined first

-- the inferred type
flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f = g  
  where g x y = f y x  

-- helps with the intuition of the flip
-- (a -> b -> c) -> (b -> (a -> c)),

-- ah a simpler implementation
flip_ :: (a -> b -> c) -> b -> a -> c  
flip_ f y x = f x y  

-- to help understand the order of application and the flipping of flip

natualOrderSubtract = (subtract) 7 5 -- -2
-- we can see that 5 is the first argument, so 5 - 7 = -2
flippedSubtract = flip_ subtract 7 5


-- Here, we take advantage of the fact that functions are curried. When we call flip' f without the parameters y and x, it will return an f that takes those two parameters but calls them flipped. Even though flipped functions are usually passed to other functions, we can take advantage of currying when making higher-order functions by thinking ahead and writing what their end result would be if they were called fully applied.

-- calling examples
flipNZip = flip' zip [1,2,3,4,5] "hello"  
zipFlipDiv =  zipWith (flip' div) [2,2..] [10,8,6,4,2]  

map_ :: (a->b) -> [a] -> [b]
map_  _ [] = []
map_ f (x:xs) = f x : map_ f xs

-- interesting uses of map and other funcs
mappedA =  map (replicate 3) [3..6]  
mappedB = map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
mappedC = map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  

-- // very insightful! on similarity between map and comprehension
-- You've probably noticed that each of these could be achieved with a list comprehension. map (+3) [1,5,3,1,6] is the same as writing [x+3 | x <- [1,5,3,1,6]]. However, using map is much more readable for cases where you only apply some function to the elements of a list, especially once you're dealing with maps of maps and then the whole thing with a lot of brackets can get a bit messy.

-- filter_ :: (p -> Bool) -> [a] -> [a]
-- ah, at first I had this type, and it wouldnt compile, i commented it out, it compiled, looked at the inferred type and ahah, its
-- -- filter_ :: (a -> Bool) -> [a] -> [a]
-- notice that the parens IS the predicate, and it takes an a

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ _ [] = []
filter_ p (x:xs) 
    | (p x) = x : filter_ p xs
    | otherwise = filter_ p xs

-- and look how its called!

trues = filter_ ( == True)  [True, False, True, False]
fives = filter_ (== 5) [3,4,5,6,5,7,5,8] 
greaterThanTen = filter_ (>10) [3,4,6,12,86,4,67]

evens = filter_ even [1..10]

-- interesting expression with the inline predicate, i like how haskell treats an empty array as null, makes an intuitive sense unlike js.
notNulls =  let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]] 

lol_a = filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
lol_b = filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"


-- Thanks to Haskell's laziness, even if you map something over a list several times and filter it several times, it will only pass over the list once.

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  

-- We didn't even need to use a finite list for our starting set. That's laziness in action again. Because we only end up using the head of the filtered list, it doesn't matter if the filtered list is finite or infinite. The evaluation stops when the first adequate solution is found.

chain 1 = [1]
chain x 
    | even x = x:chain (x `div` 2)
    | odd x = x:chain (x*3 + 1)


chain_ x 
    | (x == 1) = [1]
    | even x = x:chain_ (x `div` 2)
    | odd x = x:chain_ (x*3 + 1)

    -- visualizing the chain 
    -- https://jsfiddle.net/naughtnowwhen/rtzvb93q/3/

    -- for all starting numbers between 1 and 100, how many chains have a length greater than 15? 

    -- my attempt
-- chainsGreater =  filter (length (map chain [1..100]) > 15)

numLongChains = length (filter isLonger (map chain [1..100]))
    where isLonger x = length x > 15

    
listOfFuns = map (*) [1..10]  
-- so this applies multiple to num so that each is partially applied, but then how to apply another number
-- to each to finish the multiplication?
-- finishApplying = listOfFuns map id id

finishMultiply [] _ = []
finishMultiply _ [] = []
finishMultiply (x:xs) (y:ys) = (x y) : finishMultiply xs ys  

finished = finishMultiply listOfFuns [10,9..1]

-- lambdas

-- ah lambda is the answer to my previous question!

numLongChainsLamba = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- fold

-- rewrite sum using fold (js reduce)

sum_ :: (a -> [a]) -> a -> [a -> [a -> [a] -> [b]]]
sum' x xs = foldl (\acc x -> acc + x) 0 xs


-- first attempt at fold look at this type error
-- * Couldn't match expected type `a -> [a]'
--                   with actual type `[a -> [a] -> [b]]'

-- oh interesting look at how the error type has recursed
-- * Couldn't match expected type `a -> [a]'
-- with actual type `[a -> [a -> [a] -> [b]]]'

-- going to experiment by putting that recursed type in my signature and expecting the error to recurse yet again


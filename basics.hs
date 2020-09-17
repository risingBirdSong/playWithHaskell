double x = x * 2

triple x = x * 3

doubleUs x y = double x + double y  

doubleifSmall x = if x > 100 then x else x * 2

arrayConcatenation = [1,2,3,4] ++ [9,10,11,12]  

addStrings = "hello" ++ " " ++ "world"  

stringsAreReallyStringArrays = ['w','o'] ++ ['o','t'] 
-- woot

strPrepend = 'a':" SMALL CAT"  
--but doesnt work with several chars to prepend

numPrepend = 5:[1,2,3,4,5]  

-- retrieve values from lists with indexes
getStr ="Steve Buscemi" !! 6 
getListNum = [9.4,33.2,96.2,11.2,23.25] !! 1  

-- was confused by this, think alphabetic order
lexicographicalOrder = [3,2,1] > [2,10,100]  

shouldBeFalse = [1,2,3] > [3,2,1]
-- yes false

trueQue =  [3,4,2] == [3,4,2111] 
-- is false,
deeplytrue = [3,4,2] == [3,4,2]

getHeadOfList = head [5,4,3,2,1] 

getTailOfList =  tail [5,4,3,2,1]

getLastOfList = last [5,4,3,2,1]  

-- takes a list and returns everything except its last element.
getAllButLAstOfList = init [5,4,3,2,1]  

-- warning head, tail, last, init on empty lists will error!

getLengthOfList = length [4,6,7,8,9,3]

--check to see if a list is null or not
nullCheckFalse = null [1,2,3] 
nullCheckTrue = null []

reversedList = reverse [1,3,5,7,9,11]

takeFive = take 5 [1,2,3,4,5,6,7,8,9,10]

takeMoreThanList =  take 5 [1,2]  

dropTest = drop 3 [8,4,2,1,5,6]

getMin = minimum [8,4,2,1,5,-9,6]  

getMax = maximum [1,9,2,3,4] 

getTotal = sum [3,6,3,4,8,12]

getProduct =   product [6,2,1,2]

-- elem takes a thing and a list of things and tells us if that thing is an element of the list. It's usually called as an infix function because it's easier to read that way.

lookForElemTrue = 4 `elem` [3,4,5,6]
lookForElemFalse = 11 `elem` [3,4,5,6]

texasRangeTest_A = [1..20]  
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
texasRangeTest_B = [(-1)..(-20)]
-- apparently cannot do texas range with negatives
texasRangeTest_C = ['K'..'Z']  
-- "KLMNOPQRSTUVWXYZ"

coolStepByTwos = [2,4..20]
coolStepByThrees = [3,6..20];

stepDownwards = [20,19..1]

-- so cool!
-- infiniteList = [2,4.. ]

aNumCycle = take 10 (cycle [1,2,3])  

stringCycle = take 7 (cycle ["lol"])

listComprehension_a = [x*2 | x <- [1..10]]  
listComprehension_b = [x*3 | x <- [10..20]]

-- ah i first though <- was lesser or equal to, but its not... this explains
-- https://stackoverflow.com/questions/28624408/equal-vs-left-arrow-symbols-in-haskell

-- var action = function() { print(3); return 5; }
--  roughly equivalent to x <- action
--  var x = action()  // output:3
listCompWithPred = [x*2 | x <- [1..10], x*2 >= 12] 

listCompInfixMod = [ x | x <- [50..100], x `mod` 7 == 0] 

-- oh thats neat
oddRange = [x | x <- [1..20], odd x]

customOddRange x xs = [x | x <- [x..xs] , odd x]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   
-- called like boomBangs [4..12]

-- multiple predicates
weCanSkipValues = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] 

drawingFromMultipleLists = [ x*y | x <- [2,4,6], y <- [3,6,9]]  
-- [6,12,18,12,24,36,18,36,54]
-- length * length -> 3 * 3 -> 9

-- we can also use a predicate
drawingFromMultipleListsWithPred = [ x*y | x <- [2,4,6], y <- [3,6,9], x*y < 24]  
-- [6,12,18,12,18]
drawingFromMultipleListsWithPredA = [ x*y | x <- [2,4,6], y <- [3,6,9], x/= 4]  
drawingFromMultipleListsWithPredB = [ x*y | x <- [2,4,6], y <- [3,6,9], x+y/= 5]  

            -- ignore \/
length' xs = sum [1 | _ <- xs]  
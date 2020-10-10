

desc_a = [10,9..1]
dsc x = [x,x-1..1]


-- priming = (\x -> filter (\y -> x (//) y (/=) (\ -> [y,y-1, 1]) ) )
-- priming = (\x -> filter (\y -> x (//) y (/= [y,y-1, 1])) )

-- question, whats the | syntax in this simple list comprehension? it looks like a guard in pattern matching
-- answer, its bind for lists...
listy = [ x | x <- [1..10]]
filtered y = [ x | x <- [1..10], y + x < 10 ]

comp_a_prime k = [ x | x <- [2..k - 1], k `mod` x == 0]

doesItGoNegative = [5,4..] -- yes it does

arrLengthCheck x = if length x > 1 then False else True

isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else if k < 1 then False else False

primeList x = filter isPrime [x,x-1..1]
ascPrime = filter isPrime [1..]
getNPrimes n 
  | n < 1 = Nothing
  | otherwise = Just (last (take n ascPrime))
-- taker n = takeWhile (length n < 6)

-- great explanation by Melissa, thanks
-- The expression is null [ x | x <- [2..k - 1], k `mod` x == 0].
-- The list comprehension is generating the list of nontrivial divisors of k; if there are no nontrivial divisors, the list is empty, and null maps this to True.

-- primes from Y Combinatorian
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

-- someNumbers = [1..100]
-- someNumbers !! 5

-- this one works but im finding sprint pretty strang and buggy to work with

-- paste this GHCI
-- someNumbers = [1..100] :: [Int]
-- someNumbers !! 5
-- :sprint someNumbers

-- outputs someNumbers = 1 : 2 : 3 : 4 : 5 : 6 : _

-- nthPrime n = primeList !! n
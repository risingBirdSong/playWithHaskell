-- https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning

twentyOne =
    let
        seven = 7
    in
        seven + seven + 7

fifteen = let five = 5 in five + five + five

afactorial :: Integer -> Integer
afactorial =
  \n ->
      if n == 1
          then
              1
          else
              n * afactorial (n - 1)

myAFact n = if n == 1 then 1 else n * myAFact (n-1)

-- https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning
-- a useful series of evaluations where factorial is called with 3

-- ( \n ->
--   if n == 1
--       then
--           1
--       else
--           n * factorial (n - 1)
-- ) 3

-- 3 * ( \n ->
--       if n == 1
--           then
--               1
--           else
--               n * factorial (n - 1)
--     ) (3 - 1)


--     3 * ( 2 * ( \n ->
--             if n == 1
--                 then
--                     1
--                 else
--                     n * factorial (n - 1)
--           ) (2 - 1)
--     )

--3 * ( 2 * ( 1 ) )
-- equals 6
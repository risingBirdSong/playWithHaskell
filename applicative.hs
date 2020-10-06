import Control.Applicative

    -- because 4 are not conained, not in functors
-- doesntWork = (+) <$> 4 <*> 4
-- this does work because both vals are in functors
applicative_a = (+) <$> [4] <*>[4] -- [8]
applicative_b = (+) <$> Just 5 <*> Just 5 -- Just 10
-- notice how when you a val and function inside of just, that basically doing the job of <$>
applicative_c = Just (5+) <*> Just 5 -- Just 10

ziplista =  getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  

-- zipWith takes a function that takes two parameters and zips two lists with it. zipWith3 takes a function that takes three parameters and zips three lists with it, and so on
zippedWith =  zipWith (+) [1,2] [3,4]
applicativeStyle = (:) <$> Just 3 <*> Just [4] 
lifted = liftA2 (:) (Just 3)(Just [4])

-- so it looks like lift is a syntactic sugar for the applicative style

sequenceA' :: (Applicative f) => [f a] -> f [a]  
sequenceA' [] = pure []  
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs 

-- ah and I thought i was overly hung up on sequenceA' but really its just doing a familiar pattern like in applicativeStyle = (:) <$> Just 3 <*> Just [4] but just abstracting it into recursive pattern.

-- sequenceA [Just 4, Just 5]
-- Just [4,5]

--I think the most common way to think about it is combining the inner effects into a single outer effect." -Arc
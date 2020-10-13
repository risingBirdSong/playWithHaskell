
iterateda = take 10 (iterate (2*) 1)
iteratedb =  take 10 (iterate (\x -> (x+3)*2) 1)

iteratedc = take 10 (iterate (\x -> (x*2 +1)) 1)

cycled = take 10 (cycle (take 5 [1,22..]))
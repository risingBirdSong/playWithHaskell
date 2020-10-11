import Data.List
descendingOrder' :: Integer -> Integer
descendingOrder' = read . reverse . sort . show

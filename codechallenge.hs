import Data.List
import Data.Function
testa =  ( head . head . sortBy (compare `on` length) . groupBy (\ x y -> (x == y))) [1,2,2,2,2,2,3,3,9]

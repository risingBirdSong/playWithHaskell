import Data.Binary
import GHC.Float
import GHC.Float.RealFracMethods
-- gotA = get 
  
convert :: Double -> Int
convert x = round x

binMult :: [Int]
binMult = (map convert (map (2**) [0,1..]))

toNumber xs =sum ((zipWith (*) binMult (reverse xs)))

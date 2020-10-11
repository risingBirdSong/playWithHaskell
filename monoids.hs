import Data.Monoid
import Data.Foldable
-- import Control.Lens
import qualified Data.Char
a = "hello" <> " world" <> mempty
b = [1] <> [2] <> mempty

c = [True] <> [False] <> mempty

d :: [Maybe Integer]
d = [Just 5] <> [Just 3] <> mempty

summed = Sum 5 <> Sum 6 <> Sum 3
-- summeda = Sum 5 ++ Sum 6 ++ Sum 3

-- So concat :: [[a]] -> [a], sum :: Num a => [a] -> a, product :: Num a => [a] -> a, and unions :: Ord k => [Map k v] -> Map k v
-- These all look like different functions right?

-- Except they're all specialized versions of fold :: Monoid a => [a] -> a
-- folded = fold

-- Analogies do not help.
-- Go back to the definition I gave.

qbf = fold ["the ", "quick ", "brown ", "fox"]

go c = if Data.Char.isAlpha c then ([c],[]) else ([],[c])
fgo x = fold $ go <$> x
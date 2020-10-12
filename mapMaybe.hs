import Data.Maybe
mappedMaybe = mapMaybe (fmap (+1)) [Just 1, Just 2, Just 3, Nothing]
module Common where

import Data.Tuple.Extra (second)

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce a = second tail . break (==a)

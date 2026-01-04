module Common where

import Data.Tuple.Extra (second)

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce a = second tail . break (==a)

-- https://hackage-content.haskell.org/package/protolude-0.3.5/docs/Protolude-Functor.html#v:-60--60--36--62--62-
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

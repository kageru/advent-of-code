{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}

import Data.Ratio
import Data.List as L
import qualified Data.Vector as V
import Data.Ord
import Data.Maybe

type Asteroid_old = (Int,Int)

data Asteroid = Asteroid Int Int deriving Show
instance Eq Asteroid where
    Asteroid a b == Asteroid c d = reduce a b == reduce c d
        where reduce x y
                | y==0&&x==0 = (0,0)
                | x==0 = (0,signum y)
                | y==0 = (signum x,0)
                | otherwise = (signum x * (abs $numerator z), signum y * (abs $denominator z))
                where z = x%y
instance Ord Asteroid where
    Asteroid a b `compare` Asteroid c d = phi a b `compare`phi c d
        where phi x y
                | x>=0 = acos (fromIntegral (-y)/sqrt(fromIntegral x^2+fromIntegral y^2))
                | otherwise = 2*pi + acos (fromIntegral (x)/sqrt(fromIntegral x^2+fromIntegral y^2))

(+|) :: Asteroid -> Asteroid -> Asteroid
(+|) (Asteroid a b) (Asteroid c d) = Asteroid (a+c) (b+d)
(-|) :: Asteroid -> Asteroid -> Asteroid
(-|) (Asteroid a b) (Asteroid c d) = Asteroid (a-c) (b-d)
neg :: Asteroid -> Asteroid
neg (Asteroid a b) = Asteroid (-a) (-b)
dist :: Asteroid -> Int
dist (Asteroid x y) = abs x + abs y



main = do
    content <- lines <$> readFile "inputtest"
    print $ parse content
    let aMax = fst $ maxInSight $ parse content
    print aMax
    let newlist = listCycle $ sort $ sortOn (dist.fromJust) $ snd $ transformCoordinates (parse content) aMax
    print $ newlist !! 199
    let newnewlist = snd $ transformCoordinates newlist (neg aMax)
    print $ newnewlist !! 0
    print $ newnewlist !! 1
    print $ newnewlist !! 2
    print $ newnewlist !! 50
    print $ newnewlist !! 100
    print $ length newnewlist



listCycle :: [Maybe Asteroid] -> [Maybe Asteroid]
listCycle [] = []
listCycle xs = nub xs ++ listCycle (xs \\ nub xs)

-- alternative rotation function that also gets different results
cycle2 :: Maybe Asteroid -> [Maybe Asteroid] -> [Maybe Asteroid]
cycle2 _ [] = []
cycle2 last (x:xs)
  | x==last&& length (filter (/=x) xs) > 0 = cycle2 x (xs++[x])
  | otherwise = x:cycle2 x xs


parse :: [String] -> [Maybe Asteroid]
parse = concat . V.toList . V.map V.toList . toAsteroids . V.map V.fromList . V.fromList
    where toAsteroids = V.imap g
          g x = V.imap (f x)
          f i j a = case a of '.' -> Nothing; _ -> Just $ Asteroid j i


--inSight :: [Maybe Asteroid] -> Int
transformCoordinates :: [Maybe Asteroid] -> Asteroid -> (Asteroid,[Maybe Asteroid])
transformCoordinates xs a = (a,[Just $ x -| a |Just x<-xs])

maxInSight :: [Maybe Asteroid] -> (Asteroid, Int)
maxInSight xs = maximumBy (comparing snd) $ map (f . transformCoordinates xs) (catMaybes xs)
    where f (x,y) = (x, pred . length . nub $ y)


-- old shit be here
--maxInSight xs = map ((\(x,y)->(x,pred . length . nub . snd $ y)). transformCoordinates xs) (catMaybes xs)
--maxInSight xs = map ((pred . length . nub . snd) . transformCoordinates xs) (catMaybes xs)

-- transform2 :: Asteroid -> [Maybe Asteroid] -> [Maybe Asteroid]
-- transform2 a xs = L.map (a -|) xs



-- maxInSight x = L.maximum $ inSight x `fmap` asts x


-- sightTransform xs = map inSight xs

-- inSight xs a = (pred . L.length) $ L.nub [case a of
--                   Just (xa,ya) -> Just (reduceTuple (x-xa,y-ya))
--                   Nothing -> Nothing |Just (x,y)<-asts xs]
--
asts :: [String] -> [Maybe Asteroid_old]
asts xxs = L.nub [if (xxs!!y)!!(x)=='.' then Nothing else Just (x,y)
  | y<-[0..(L.length $ L.head xxs) -1], x<-[0..L.length xxs -1] ]

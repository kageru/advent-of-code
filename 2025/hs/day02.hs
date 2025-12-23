import Data.List.Split

type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  print input
  putStrLn $ "Part 1: " ++ show (solve isValid input)
  putStrLn $ "Part 2: " ++ show (solve isValid2 input)

parse :: String -> Input
parse = map (\line -> let (a, b) = break (== '-') line in (read a, read $ tail b)) . splitOn ","

isValid :: Int -> Bool
isValid n = a == b
  where s = show n
        (a, b) = splitAt (length s `div` 2) s

isValid2 :: Int -> Bool
isValid2 n = any (chunksAreEqual s) [1..length s `div` 2]
  where s = show n

chunksAreEqual :: String -> Int -> Bool
chunksAreEqual s len = all (uncurry (==)) $ zip =<< tail $ chunksOf len s

solve :: (Int -> Bool) -> Input -> Int
solve f = sum . concatMap (filter f . \(a, b) -> [a .. b])

type Input = [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (solve parseLine input)
  putStrLn $ "Part 2: " ++ show (solve parseLine2 input)

parse :: String -> Input
parse = lines

solve :: (String -> [Int]) -> Input -> Int
solve f = length 
  . filter (== 0) 
  . scanl (\acc x -> (acc + x) `mod` 100) 50 
  . concatMap f

parseLine :: String -> [Int]
parseLine (first:rest) = [f $ read rest]
  where f = if first == 'R' then id else negate

parseLine2 :: String -> [Int]
parseLine2 (first:rest)
  | first == 'R' = parsed 1
  | otherwise = parsed (-1)
  where parsed = replicate (read rest)

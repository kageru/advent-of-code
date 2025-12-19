import Data.List.Split

type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  print input
  putStrLn $ "Part 1: " ++ show (part1 input)

parse :: String -> Input
parse = map (\line -> let (a, b) = break (== '-') line in (read a, read $ tail b)) . splitOn ","

isValid :: Int -> Bool
isValid n = a == b
  where s = show n
        (a, b) = splitAt (length s `div` 2) s

part1 :: Input -> Int
part1 = sum . concatMap (filter isValid . \(a, b) -> [a .. b])

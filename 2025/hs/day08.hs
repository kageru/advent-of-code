import Common

type Input = [Int]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map read . lines

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined

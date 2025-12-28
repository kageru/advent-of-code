import Data.Tuple.Extra
import Data.List
import Common

type Input = (Ranges, [Int])
type Ranges = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (uncurry part1 input)
  putStrLn $ "Part 2: " ++ show (part2 $ fst input)

parse :: String -> Input
parse = (map parseRange *** map read) . splitOnce "" . lines
  where parseRange = both read . splitOnce '-'

part1 :: Ranges -> [Int] -> Int
part1 rs = length . filter (`inAnyRange` rs)
  where inAnyRange = any . inRange
        inRange i (a, b) = a <= i && i <= b

part2 :: Ranges -> Int
part2 = sum . map (\(a, b) -> b - a + 1) . mergeRanges . sortOn fst

mergeRanges :: Ranges -> Ranges
mergeRanges [x] = [x]
mergeRanges (prev@(s1, e1) : next@(s2, e2) : rest)
  | s2 <= e1 + 1 = mergeRanges $ (s1, max e1 e2):rest
  | otherwise    = prev : mergeRanges (next:rest)

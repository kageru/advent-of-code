import Data.List.Extra
import Data.Tuple.Extra ((&&&))
import Common

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

part1 :: String -> Int
part1 s = sum $ zipWith parseOp ops $ transpose $ read <<$>> ns
  where (ops : ns) = reverse $ map (filter (/="") . split (==' ')) $ lines s

parseOp :: String -> [Int] -> Int
parseOp "+" = sum
parseOp "*" = product

part2 :: String -> Int
part2 = fst . foldl addLine (0, []) . map (read . dropEnd 1 &&& last) . reverse . filter (not . all (==' ')) . transpose . lines

addLine :: (Int, [Int]) -> (Int, Char) -> (Int, [Int])
addLine (acc, chunk) (n, ' ') = (acc, n:chunk)
addLine (acc, chunk) (n, op) = (acc + chunkTotal, [])
  where chunkTotal = parseOp [op] $ n:chunk

import Data.List.Extra
import Data.Char

type Input = [String]

main :: IO ()
main = do
  input <- lines <$> getContents
  putStrLn $ "Part 1: " ++ show (solve 1 input)
  putStrLn $ "Part 2: " ++ show (solve 11 input)

solve :: Int -> Input -> Int
solve i = sum . map joltage
  where joltage s = read $ map (s !!) $ indices s 0 i

indices :: String -> Int -> Int -> [Int]
indices s start (-1) = []
indices s start end = indexOfMax:indices s (indexOfMax + 1) (end - 1)
  where indexOfMax = fst $ maximumOn snd $ drop start $ dropEnd end $ zip [0..] s

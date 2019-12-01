import System.IO  
import Text.Printf

main :: IO ()
main = do
  content <- getContents
  let input = map read (lines content)
  printf "Part 1: %d\n" (fuel input)
  printf "Part 2: %d\n" (fuelrec input)

fuel :: [Integer] -> Integer
fuel xs = sum (map (subtract 2 . (`div` 3)) xs)

fuelrec :: [Integer] -> Integer
fuelrec xs = sum (map f xs) - sum xs where
    f x | x > 0 = x + f ((subtract 2 . (`div` 3)) x)
        | otherwise = 0

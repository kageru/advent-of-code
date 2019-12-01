import System.IO  
import Text.Printf

main :: IO ()
main = do
  content <- getContents
  let input = map read (lines content)
  printf "Part 1: %d\n" (part1 input)
  printf "Part 2: %d\n" (part2 input)

part1 :: [Integer] -> Integer
part1 xs = sum (map cost xs)

part2 :: [Integer] -> Integer
part2 xs = sum (map f xs) - sum xs where
    f x | x > 0 = x + f (cost x)
        | otherwise = 0

cost :: Integer -> Integer
cost x = div x 3 - 2

import Data.Vector (Vector, fromList, findIndex, elemIndex, (!), (!?))
import Data.Tuple.Extra ((&&&))
import Data.Maybe (fromJust)
import qualified Data.Map as M 

type Input = (P, Vector (Vector Char))
type P = (Int, Int)

main :: IO ()
main = do
  input <- parse <$> getContents
  let (splits, total) = part2 input M.empty
  putStrLn $ "Part 1: " ++ show (length splits)
  putStrLn $ "Part 2: " ++ show total

parse :: String -> Input
parse s = (position 'S' &&& id) v
  where v = fromList . map fromList $ reverse $ lines s

position :: Eq a => a -> Vector (Vector a) -> (Int, Int)
position e m = (fromJust $ elemIndex e (m ! y), y)
  where Just y = findIndex (elem e) m

part2 :: Input -> M.Map P Int -> (M.Map P Int, Int)
part2 ((x, y), m) cache
  | Just i <- (M.!?) cache (x, y) = (cache, i)
  | otherwise = case (!? x) =<< (m !? y) of
      Nothing -> (cache, 1)
      Just '^' -> let
        (leftCache, leftNum) = part2 ((x - 1, y), m) cache
        (rightCache, rightNum) = part2 ((x + 1, y), m) leftCache
        total = leftNum + rightNum
        in (M.insert (x, y) total rightCache, total)
      Just e -> part2 ((x, y - 1), m) cache

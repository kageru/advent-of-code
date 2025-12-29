import Data.Vector (Vector, fromList, findIndex, elemIndex, (!), (!?))
import Data.Tuple.Extra ((&&&))
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Monad.State.Strict (State, get, modify, runState)

type P = (Int, Int)
type Input = (P, Vector (Vector Char))
type Cache = (M.Map P Int)

main :: IO ()
main = do
  input <- parse <$> getContents
  let (total, splits) = runState (solve input) M.empty
  putStrLn $ "Part 1: " ++ show (length splits)
  putStrLn $ "Part 2: " ++ show total

parse :: String -> Input
parse s = (position 'S' &&& id) v
  where v = fromList . map fromList $ reverse $ lines s

position :: Eq a => a -> Vector (Vector a) -> (Int, Int)
position e m = (fromJust $ elemIndex e (m ! y), y)
  where Just y = findIndex (elem e) m

solve :: Input -> State Cache Int
solve ((x, y), m) = do
  cache <- get
  maybe fallback pure $ cache M.!? (x, y)
  where
    fallback = maybe (pure 1) nextStep $ (!? x) =<< (m !? y)
    nextStep '^' = do
      total <- sum <$> mapM (\f -> solve ((f x, y), m)) [pred, succ]
      modify (M.insert (x, y) total)
      pure total
    nextStep _ = solve ((x, y - 1), m)

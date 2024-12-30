----
-- A very interesting exercise in folds. https://adventofcode.com/2024/day/10
--
-- It is kind of unfortunate that the given given input is small enough that no optimization is
-- needed for covering parts of tree multiple times. If some tree'a tails overlap, the computation is
-- repeated unnecessarily, but the code still runs fast enough.
import Data.Char (digitToInt)
import Data.List (elemIndices)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, fromLists, getElem, safeGet)
import Data.Monoid (Sum (Sum), getSum)
import qualified Data.Set as Set

type Point = (Int, Int)
data Tree a = Node a [Tree a] deriving (Foldable, Show)

expandTree :: (Num a, Eq a) => Matrix a -> Point -> Tree Point
expandTree board p@(x, y) = Node p $ expandTree board <$> nexts
 where
  nexts = move <$> filter canGo directions
  directions = [(0, 1), (0, -1), (-1, 0), (1, 0), (0, 0)]
  canGo (dx, dy) = safeGet (x + dx) (y + dy) board == Just (currentHeight + 1)
  move (dx, dy) = (x + dx, y + dy)
  currentHeight = getElem x y board

-- Part 1 asks for the number of '9's reachable from each trailhead
part1 :: Matrix Int -> Tree Point -> Int
part1 board = countUnique . foldMap f
 where
  f (x, y) = if getElem x y board == 9 then [(x, y)] else []
  countUnique = Set.size . Set.fromList

-- Part 2 asks for the number of unique paths to all peaks
part2 :: Matrix Int -> Tree Point -> Int
part2 board = getSum . foldMap (Sum . f)
 where
  f (x, y) = if getElem x y board == 9 then 1 else 0

findHeads :: [[Int]] -> [Point]
findHeads rows = do
  (row, rowTails) <- zip [0 ..] $ (0 `elemIndices`) <$> rows
  column <- rowTails
  return (row + 1, column + 1)

main :: IO ()
main = do
  f <- readFile "10.in"
  let rows = map (map digitToInt) . splitOn "\n" . withoutLast $ f
      matrix = fromLists rows
      trails = expandTree matrix <$> findHeads rows
  putStr "Part 1: "
  print . sum $ part1 matrix <$> trails
  putStr "Part 2: "
  print . sum $ part2 matrix <$> trails

withoutLast :: [a] -> [a]
withoutLast (_ : []) = []
withoutLast (a : as) = a : (withoutLast as)
withoutLast [] = []

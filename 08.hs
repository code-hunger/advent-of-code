import Control.Monad (guard)
import Data.List (findIndices)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Set (fromList, size, unions)

type Board = [[Char]]
type Point = (Int, Int)

-- Finds the next external point on the a-b line that is twice as far from the
-- first point than from the second.
shootExt :: Point -> Point -> Point
shootExt (x1, y1) (x2, y2) = (x2 + (x2 - x1), y2 + (y2 - y1))

-- Finds the inner point on the a-b line segment that is twice as far from the
-- first point than from the second.
shootInt :: Point -> Point -> Maybe Point
shootInt (x1, y1) (x2, y2) =
  if div3 (x1 - x2) && div3 (y1 - y2)
    then Just (x1 + (x2 - x1) `div` 3, y1 + (y2 - y1) `div` 3)
    else Nothing
 where
  div3 a = a `rem` 3 == 0

-- Finds the next points on the a-b line with integer coordinates, starting
-- from the first given point in the given direction (+1 or -1).
shootSteps :: Point -> Point -> Int -> [Point]
shootSteps (x1, y1) (x2, y2) direction = shootStep . (direction *) <$> [0 ..]
 where
  shootStep n =
    let d = (x2 - x1) `gcd` (y2 - y1)
        xd = (x2 - x1) `div` d
        yd = (y2 - y1) `div` d
     in (x1 + n * xd, y1 + n * yd)

-- as it says
inside :: Board -> Point -> Bool
inside board (x, y) = x >= 0 && x < boundX && y >= 0 && y < boundY
 where
  boundX = length board
  boundY = length $ head board -- assuming square board!

-- Given a function that produces stuff from pairs of points,
-- feeds that function all pairs of the given type and returns a list of the results.
genFromPairs :: Board -> Char -> (Point -> Point -> r) -> [r]
genFromPairs board t f = [f a b | a <- antennas, b <- antennas, a /= b]
 where
  antennas = do
    (row, rowAntennas) <- zip [0 ..] $ findIndices (== t) <$> board
    column <- rowAntennas
    return (row, column)

-- Solves the first part of the problem.
-- Returns all antinodes of the given antena type t.
shootsOfType :: Board -> Char -> [Point]
shootsOfType board t = internalShots ++ externalShots
 where
  internalShots = catMaybes $ genFromPairs board t shootInt
  externalShots = inside board `filter` genFromPairs board t shootExt

-- Solves the second part of the problem.
-- Returns all antinodes of the given antena type t.
-- I know I do it twice and that unnecessary (genFromPairs gives us each point
-- pair twice - (a,b) and (b,a)), but that's the easiest.
shootsOfType' :: Board -> Char -> [Point]
shootsOfType' board t = concat $ genFromPairs board t genFromPair
 where
  genFromPair a b =
    -- advances forward and backward until it gets out of the board
    let shootStepsInBoard = takeWhile (inside board) . shootSteps a b
     in shootStepsInBoard 1 ++ shootStepsInBoard (-1)

-- Use shootsOfTypes or hootsOfType' for the first and second part of the problem, respectively.
solveTask :: Board -> Int
solveTask board = size . unions $ fromList . shootsOfType' board <$> antennaTypes
 where
  antennaTypes = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

unTail :: [a] -> [a]
unTail [a] = []
unTail [] = error "unTail on empty list"
unTail (a : as) = a : unTail as

main :: IO ()
main = solveTask . unTail <$> splitOn "\n" <$> readFile "08.in" >>= print

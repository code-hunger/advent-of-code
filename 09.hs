import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Char (ord)
import Data.List (foldl', reverse)

data Pos = Block | Space

instance Show Pos where
  show Block = "#"
  show Space = "_"

type FileDesc = (Int, Int)

positions :: [Int] -> [Pos]
positions disk = take compactLen $ go disk
 where
  go (file : space : rest) = (replicate file Block) ++ (replicate space Space) ++ go rest
  go [file] = replicate file Block
  go [] = []

  compactLen = sum $ snd <$> files disk :: Int

checksum :: [Int] -> Int
checksum = sum . map (\(pos, file) -> pos * file) . zip [0 ..]

files :: [Int] -> [FileDesc]
files disk = go disk 0
 where
  go (file : _ : rest) n = (n, file) : go rest (n + 1)
  go [file] n = [(n, file)]
  go [] _ = []

result :: [Int] -> [Int]
result disk = evalState (mapM f (positions disk)) (files disk, reverse $ files disk)
 where
  f :: Pos -> State ([FileDesc], [FileDesc]) Int
  f pos = do
    (forward, backward) <- get
    let (multiple, forward', backward') = doStep pos forward backward
    put (forward', backward')
    return (multiple)

  doStep :: Pos -> [FileDesc] -> [FileDesc] -> (Int, [FileDesc], [FileDesc])
  doStep Block ((fileIndex, fileRemain) : fs) bs =
    if fileRemain <= 1
      then (fileIndex, fs, bs)
      else (fileIndex, (fileIndex, fileRemain - 1) : fs, bs)
  doStep Space fs ((fileIndex, fileRemain) : bs) =
    if fileRemain <= 1
      then (fileIndex, fs, bs)
      else (fileIndex, fs, (fileIndex, fileRemain - 1) : bs)
  doStep Space fs [] = (0, [], [])

main = do
  f <- readFile "09.in"
  putStr "Part 1: "
  print . checksum . result . map toInt . filter (/= '\n') $ f
 where
  toInt c = ord c - ord '0'

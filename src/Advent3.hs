module Advent3
    ( advent3_1, advent3_2
    ) where

data SquareSide = Dn | Lf | Up | Rt deriving (Show, Eq)

squaredOdds = [(i, odd i, (odd i) ^ 2) | i <- [0..]] where odd n = 2 * n + 1

hiBound n = head $ dropWhile (\(_, _, x) -> n > x) squaredOdds

whichSide n (i, biggestOdd, biggestOddSquared)
  | (biggestOddSquared - biggestOdd) < n                        = Dn
  | (biggestOddSquared - biggestOdd - (biggestOdd - 1)) < n     = Lf
  | (biggestOddSquared - biggestOdd - (biggestOdd - 1) * 2) < n = Up
  | (biggestOddSquared - biggestOdd - (biggestOdd - 1) * 3) < n = Rt
  | otherwise = error $ "ERROR: n: " ++ (show n) ++ " sq: " ++ (show (i, biggestOdd, biggestOddSquared))

coordinates n (numSeq, biggestOdd, biggestOddSquared) side
  | side == Dn = (n - middleDn, numSeq)
  | side == Lf = (- numSeq,     n - middleLf)
  | side == Up = (middleUp - n, - numSeq)
  | side == Rt = (numSeq,       middleRt - n)
  where middleDn = biggestOddSquared - (biggestOdd - 1) * 0 - (biggestOdd `div` 2)
        middleLf = biggestOddSquared - (biggestOdd - 1) * 1 - (biggestOdd `div` 2)
        middleUp = biggestOddSquared - (biggestOdd - 1) * 2 - (biggestOdd `div` 2)
        middleRt = biggestOddSquared - (biggestOdd - 1) * 3 - (biggestOdd `div` 2)

-- advent3_2

data Cell = Cell {
  cellGenPos :: Int,
  cellX :: Int,
  cellY :: Int,
  cellValue :: Int
} deriving (Show)

generator 0 = Cell {
  cellGenPos = 0,
  cellX = 0,
  cellY = 0,
  cellValue = 1
}

-- Answers

advent3_1 = crsum cr
  where hb = hiBound input
        ws = whichSide input hb
        cr = coordinates input hb ws
        crsum (x, y) = (abs x) + (abs y)

advent3_2 = 0

-- Input

input = 277678 

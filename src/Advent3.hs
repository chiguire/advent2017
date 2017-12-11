module Advent3
    ( advent3_1, advent3_2
    ) where

data SquareSide = Rt | Up | Lf | Dn deriving (Show, Enum, Bounded, Eq)

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

sidesSequence = [toEnum (i `rem` (fromEnum (maxBound :: SquareSide) + 1)) :: SquareSide | i <- [0..]]

integerSequence = concat [replicate 2 i | i <- [1..]]

instructions = zip integerSequence sidesSequence

directionSequence = concat [replicate n d | (n,d) <- instructions]

move (x, y) Rt = (x + 1, y)
move (x, y) Up = (x, y - 1)
move (x, y) Lf = (x - 1, y)
move (x, y) Dn = (x, y + 1)

start = (0, 0)

coordinatesSequence n = foldl (\c d -> move c d) start (take n directionSequence)

-- Answers

advent3_1 = crsum cr
  where hb = hiBound input
        ws = whichSide input hb
        cr = coordinates input hb ws
        crsum (x, y) = (abs x) + (abs y)

advent3_2 = take 20 [(i, coordinatesSequence i) | i <- [0..]]

-- Input

input = 277678 

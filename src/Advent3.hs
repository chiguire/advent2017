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

coordinatesSequence = scanl (\c d -> move c d) (0,0) directionSequence

move (x, y) Rt = (x + 1, y)
move (x, y) Up = (x, y - 1)
move (x, y) Lf = (x - 1, y)
move (x, y) Dn = (x, y + 1)

getValue :: Maybe Int -> Int
getValue Nothing = 0
getValue (Just n) = n

nextValue :: (Int, Int) -> [((Int, Int), Int)] -> Int
nextValue (x, y) list = sum $ map (getValue . (flip lookup list)) neighbors
  where neighbors = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

accumulateCoordinates :: [(Int, Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
accumulateCoordinates (nextCoord:coordsSeq) list =
  nextCoordVal : accumulateCoordinates coordsSeq (nextCoordVal : list)
  where nextCoordVal = (nextCoord, nextVal)
        nextVal = nextValue nextCoord list

-- Answers

advent3_1 = crsum cr
  where hb = hiBound input
        ws = whichSide input hb
        cr = coordinates input hb ws
        crsum (x, y) = (abs x) + (abs y)

advent3_2 = head $ dropWhile (\(c, v) -> v < input) $ accumulateCoordinates (drop 1 coordinatesSequence) [((0, 0), 1)]

-- Input

input = 277678 

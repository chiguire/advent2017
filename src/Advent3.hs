module Advent3
    ( advent3_1, advent3_2
    ) where

data SquareSide = Dn | Lf | Up | Rt deriving (Show)

squaredOdds = [(odd i, (odd i) ^ 2) | i <- [0..]] where odd n = 2 * n + 1

hiBound n = head $ dropWhile (\x -> n > (snd x)) squaredOdds

whichSide n (biggestOdd, biggestOddSquared)
  | (biggestOddSquared - biggestOdd) < n                        = Dn
  | (biggestOddSquared - biggestOdd - (biggestOdd - 1)) < n     = Lf
  | (biggestOddSquared - biggestOdd - (biggestOdd - 1) * 2) < n = Up
  | (biggestOddSquared - biggestOdd - (biggestOdd - 1) * 3) < n = Rt

-- coordinates (biggestOdd, biggestOddSquare) side
--  | Dn = 

-- Answers

advent3_1 = ws --  coordinates hb ws
  where hb = hiBound input
        ws = whichSide input hb

advent3_2 = 0

-- Input

input = 277678 

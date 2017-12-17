module Advent6
    ( advent6_1, advent6_2
    ) where

winnerBankIndex :: [Int] -> Int
winnerBankIndex (x:xs) = winnerBankIndex' xs 1 0 x
  where winnerBankIndex' [] _ maxIndex _ = maxIndex
        winnerBankIndex' (x:xs) currentIndex maxIndex maxVal
          | x > maxVal = winnerBankIndex' xs (currentIndex + 1) currentIndex x
          | otherwise  = winnerBankIndex' xs (currentIndex + 1) maxIndex     maxVal

getAllBlocks :: [Int] -> Int -> ([Int], Int)
getAllBlocks list index = (rebuiltList list index, list !! index)
  where rebuiltList l i = concat [take i l, [0], drop (i+1) l]

distributeBlocks :: ([Int], Int) -> Int -> [Int]
distributeBlocks (list, blocksLeft) startingIndex = distributeBlocks' (list, blocksLeft) startingIndex
  where distributeBlocks' (l, 0) _ = l
        distributeBlocks' (l, blocksLeft) currentIndex = distributeBlocks' ((blockAddedToList l currentIndex), blocksLeft - 1) (indexIncremented currentIndex l)
        blockAddedToList l i = concat [take i l, [(l!!i) + 1], drop (i+1) l]

indexIncremented :: Int -> [Int] -> Int
indexIncremented i l = (i + 1) `mod` (length l)

robinHood :: [Int] -> [Int]
robinHood memory = distributeBlocks removeBlocks (indexIncremented winnerIndex memory)
  where winnerIndex = winnerBankIndex memory
        removeBlocks = getAllBlocks memory winnerIndex

allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs

distributeCycles initialBlocks start = [take x $ drop start $ iterate robinHood initialBlocks | x <- [1..]]
    
-- Answers

advent6_1 = length $ takeWhile (allDifferent) $ distributeCycles io 0
  where io = map read $ words input
        
advent6_2 = length $ takeWhile (allDifferent) $ distributeCycles io 6680
  where io = map read $ words input

-- Input

input :: String
input = "4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3"

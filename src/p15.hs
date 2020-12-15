module Main where

import Data.List
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    print sol1 -- 240
    print sol2 -- 505 ... brute force ... takes a couple mins; be sure to compile instead of run unoptimized

-- Would be interesting to study the sequence further.0

sol1 :: Integer
sol1 = finalNum where (_, finalNum, _) = head . drop (2020 - inputLen) . play $ setup input

sol2 :: Integer
sol2 = finalNum where (_, finalNum, _) = head . drop (30000000 - inputLen) . play $ setup input

setup :: [Integer] -> (Hist, Integer, Integer)
setup preamble = (Map.fromList $ zip (init preamble) [1..], last preamble, fromIntegral $ length preamble)

play :: (Hist, Integer, Integer) -> [(Hist, Integer, Integer)]
play turn0 = iterate step turn0

step :: (Hist, Integer, Integer) -> (Hist, Integer, Integer)
step (hist, prevNum, turn) =
    case Map.lookup prevNum hist of
        Nothing -> (pushNum prevNum turn hist, 0, turn+1)
        Just oldTurn -> let newNum = turn - oldTurn in (pushNum prevNum turn hist, newNum, turn+1)

pushNum :: Integer -> Integer -> Hist -> Hist
pushNum n turn hist = Map.alter f n hist where
    f Nothing = Just turn
    f _ = Just turn

-- Map whose key is the number spoken, and value is the prev turn it was spoken.
type Hist = Map.Map Integer Integer

inputLen :: Int
inputLen = length input

input :: [Integer]
input = [14,8,16,0,1,17]

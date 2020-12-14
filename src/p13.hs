{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Data.List
import "split" Data.List.Split
import "arithmoi" Math.NumberTheory.Euclidean (extendedGCD) 

main = do
    print sol1 -- 246
    print sol2 -- 939490236001473

sol1 = let (bus, departTime) = nextBusDeparture goalDeparture activeSched in (departTime - goalDeparture) * bus
    where
        goalDeparture :: Integer
        goalDeparture = read $ input!!0

sol2 = period - phase
    where
        (period, phase) = foldr1 combinePhasedRotations challengeSched

nextBusDeparture :: Integer -> [Integer] -> (Integer, Integer)
nextBusDeparture t0 busses = minimumBy (\(_,a) (_,b) -> compare a b) (zip busses $ nextBusDeparture' t0 `fmap` busses)

nextBusDeparture' :: Integer -> Integer -> Integer
nextBusDeparture' t0 bus
    | t0 < 0 = 0
    | otherwise = (((t0 - 1) `div` bus) + 1) * bus

challengeSched :: [(Integer, Integer)] -- (bus,t0) t0 is offset from start time
challengeSched = [(bus,t0) | (s,t0) <- zip sched [0..], s /= "x", let bus = read s :: Integer]

activeSched :: [Integer]
activeSched = [read s | s <- sched, s /= "x"]

sched :: [String]
sched = splitOn "," (input!!1)

-- Learned a lot about the Euclidean Algorithm and going further to solve problems where we need to think
-- about sonchronizing periodic processes that start or end at offsets from each other. This was neither
-- easy nor obvious for me even when handed an implementation, as it involves experience with discrete
-- mathematics & number theory to even know where to begin.
--
-- Specifically, combinePhasedRotations below is not mine; it is transcribed from a python implementation
-- by Eric Langlois on the Math Stack Exhange along with an excellent organized/clear explanation of what is
-- going on:
--
--   https://math.stackexchange.com/q/2218763
--
-- I had to be very patient with myself and make the time to explore what was even going on. :)

combinePhasedRotations :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combinePhasedRotations (periodA, phaseA) (periodB, phaseB) =
    let
        (gcd, s, t) = extendedGCD periodA periodB
        phaseDifference = phaseA - phaseB
        (pdMult, pdMod) = divMod phaseDifference gcd
        combinedPeriod = periodA `div` gcd * periodB
        combinedPhase = (phaseA - s * pdMult * periodA) `mod` combinedPeriod
    in
        if pdMod /= 0 then error "no solution" else (combinedPeriod, combinedPhase)

input :: [String]
input =
    [ "1000066"
    , "13,x,x,41,x,x,x,37,x,x,x,x,x,659,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,29,x,409,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17" ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Numeric
import Text.Parsec
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    s <- readStatements
    print $ sol1 s -- 15514035145260
    print $ sol2 s -- 3926790061594

sol1 :: [Stmt] -> Int
sol1 statements = Map.foldr (+) 0 mem
    where (ProgState _ mem) = runProg step initProgState statements

sol2 :: [Stmt] -> Int
sol2 statements = Map.foldr (+) 0 mem
    where (ProgState _ mem) = runProg stepMemDecoder initProgState statements

initProgState :: ProgState
initProgState = ProgState "" Map.empty

runProg :: (ProgState -> Stmt -> ProgState) -> ProgState -> [Stmt] -> ProgState
runProg step init statements = foldl' step init statements

step :: ProgState -> Stmt -> ProgState
step (ProgState _ memory) (SetMask mask) = ProgState mask memory
step (ProgState mask memory) (SetMem addr val) = ProgState mask $ Map.insert addr (applyMask mask val) memory

stepMemDecoder :: ProgState -> Stmt -> ProgState
stepMemDecoder (ProgState _ memory) (SetMask mask) = ProgState mask memory
stepMemDecoder (ProgState mask memory) (SetMem addr val) = ProgState mask $ Map.union allWrites memory
    where allWrites = Map.fromList $ zip (memDecoderAddrs mask addr) (repeat val)


memDecoderAddrs :: String -> Int -> [Int]
memDecoderAddrs mask addr0 = fmap readbin $ crossCombo $ zipWith applyAddrMaskBit mask (leftpad (length mask) '0' $ showbin addr0)

-- There is probably a standard Haskell function for doing this already,
-- but crossPermute is my hand rolled solution to the problem of creating 
-- a set of combinations at every non-singular element (for the memory floating
-- memory combos in part 2.)
--
-- For example:
--
-- crossCombo ["0","1","01","1","1","01","1"]
-- = ["0101101"
--   ,"0101111"
--   ,"0111101"
--   ,"0111111"]
--
-- Want to achieve evaluation similar to this:
--
-- cc ["1"] = ["1"]
--
-- cc ["01","1"] 
--     = concat [(fmap ('0':) $ cc ["1"]), (fmap ('1':) $ cc ["1"])]
--     = concat [(fmap ('0':) ["1"]), (fmap ('1':) ["1"])]
--     = concat [["01"],["11"]]
--     = ["01", "11"]
--
crossCombo :: [[a]] -> [[a]]
crossCombo [] = [[]]
crossCombo (xs:xss) = [x:ys | x <- xs, ys <- crossCombo xss]

applyAddrMaskBit :: Char -> Char -> [Char]
applyAddrMaskBit '0' x = [x]
applyAddrMaskBit '1' _ = ['1']
applyAddrMaskBit _ _ = ['0','1']

data ProgState = ProgState !String !Memory deriving (Show, Eq)
type Memory = Map.Map Int Int

applyMask :: (Integral a, Show a) => String -> a -> a
applyMask mask val = readbin $ zipWith applyMaskBit mask (leftpad (length mask) '0' $ showbin val)

applyMaskBit :: Char -> Char -> Char
applyMaskBit '0' _ = '0'
applyMaskBit '1' _ = '1'
applyMaskBit _ x = x

showbin :: (Integral a, Show a) => a -> String
showbin n = showIntAtBase 2 ("01"!!) n ""

readbin :: Integral a => String -> a
readbin s = sum $ zipWith (*) [2^bit | bit <- [0..]] [charbit c | c <- reverse s]

charbit :: Integral a => Char -> a
charbit '0' = 0
charbit '1' = 1
charbit c = error $ "bad bit: " ++ show c

leftpad :: Int -> a -> [a] -> [a]
leftpad n val xs = replicate (n - length xs) val ++ xs

readStatements :: IO [Stmt]
readStatements = do
    let name = "p14.txt"
    input <- lines <$> readFile name
    case sequence $ parse stmt name <$> input of
        Right x -> pure x
        Left e -> error (show e)

stmt :: Parsec String () Stmt
stmt = 
    try mask <|> mem

mask :: Parsec String () Stmt
mask = do
    string "mask = "
    SetMask <$> many1 anyChar

mem :: Parsec String () Stmt
mem = do
    string "mem["
    addr <- read <$> many1 digit
    string "] = "
    val <- read <$> many1 digit
    pure $ SetMem addr val

data Stmt
    = SetMask !String
    | SetMem !Int !Int
    deriving (Eq, Show)

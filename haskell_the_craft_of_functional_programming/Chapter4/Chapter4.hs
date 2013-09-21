module Chapter4 where
import Prelude
import Test.QuickCheck

maxThree :: Integer -> Integer -> Integer -> Integer
maxFourModelOfThree, maxFourUseMax, maxFourUseBoth :: Integer -> Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c

maxFourModelOfThree a b c d
    | a > b && a > c && a > d = a
    | b > a && b > c && b > d = b
    | c > a && c > b && c > d = c
    | d > a && d > b && d > c = d

maxFourUseMax a b c d = max d (max (max a b) c)
maxFourUseBoth a b c d = max a (maxThree d b c)

between :: Integer -> Integer -> Integer -> Bool

between a b c
    | a <= b && b <= c = True
    |otherwise = False

howManyEqual :: Integer -> Integer -> Integer -> Integer

howManyEqual a b c
    | a == b && a == c = 3
    | a == c || a == b = 2
    | otherwise = 0

howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEqual a b c d
    | a == b && a == c && a == d = 4
    | a == b && a == c = 3
    | a == c && a == d = 3
    | b == c && b == d = 3
    | a == b || a == c || a == d || b == c || b == d = 2
    | otherwise = 0

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)


maxThreeOccurs a b c =
    (maximal a b c, occurs (maximal a b c) a b c)
    where
        maximal a b c = max (max a b) c
        occurs a b c d
            | a == b && a == c && a == d = 3
            | a == c && a == d = 2
            | a == b && a == d = 2
            | a == b && a == c = 2
            | otherwise = 1

data GameResult = Win |
              Loss |
              Draw
data Move = Rock | Paper | Scissors
            deriving(Show, Eq)

beat, lose :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

outcome :: Move -> Move -> GameResult
outcome a b
    |a == beat b = Win
    |a == lose b = Loss
    |otherwise = Draw

data Temp = Cold | Hot
            deriving (Eq, Show, Ord)

data Season = Winter | Fall | Summer | Spring

seasonalTemperature :: Season -> Temp
seasonalTemperature Summer = Hot
seasonalTemperature _ = Cold

rangeProduct :: Int -> Int -> Int
rangeProduct m n
    |m < n = m * (rangeProduct (m+1) n)
    |m == n = n
    |otherwise = rangeProduct n m

fac :: Int -> Int
fac m = rangeProduct 1 m

recursiveMultiply :: Int -> Int -> Int
recursiveMultiply a b
    |a == 1 = b
    |otherwise = b + recursiveMultiply (a-1) b



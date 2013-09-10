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

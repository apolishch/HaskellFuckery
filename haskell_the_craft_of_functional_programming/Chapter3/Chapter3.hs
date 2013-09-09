module Chapter3 where
import Prelude hiding(max, min)
import Data.Char
import Test.QuickCheck

exOr,exOr2, myAnd, myOr, nAnd1, nAnd2 :: Bool -> Bool -> Bool
exOr x y = (x == True && y == False) || (x == False && y == True)

exOr2 True True = False
exOr2 True False = True
exOr2 False True = True
exOr2 False False = False

myAnd True x = x
myAnd False x = False

myOr True x = True
myOr False x = x

nAnd1 True x = not x
nAnd1 False x = True

nAnd2 x y = not (x && y)

prop_exOrs, prop_myAnd, prop_myOr, prop_nAnds :: Bool -> Bool -> Bool

prop_exOrs x y = exOr x y == exOr2 x y
prop_myAnd x y = myAnd x y == (x && y)
prop_myOr x y = myOr x y == (x || y)
prop_nAnds x y = nAnd1 x y == nAnd2 x y

threeDifferent :: Integer -> Integer -> Integer -> Bool
fourDifferent, fourDifferentUsingThree :: Integer -> Integer -> Integer -> Integer -> Bool

threeDifferent x y z = not (x == y || y == z || x == z)

fourDifferent a b c d = not (a == b || a == c || a == d || b == c || b == d || c == d)
fourDifferentUsingThree a b c d= not (a == b || a == c || a == d) && threeDifferent b c d

prop_threeDifferent :: Integer -> Integer -> Integer -> Bool
prop_fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool

prop_fourDifferent a b c d = fourDifferent a b c d == fourDifferentUsingThree a b c d
prop_threeDifferent a b c = threeDifferent a b c == (a /= b && a /= c && b /= c)

min :: Int -> Int -> Int
minThree :: Int -> Int -> Int -> Int
min a b
    |a <= b = a
    |otherwise = b

minThree a b c = min (min a b) (min a c)

prop_min1, prop_min2 :: Int -> Int -> Bool
prop_minThree1, prop_minThree2 :: Int -> Int -> Int -> Bool

prop_min1 a b = a >= (min a b) || b >= (min a b)
prop_min2 a b = a == (min a b) || b == (min a b)

prop_minThree1 a b c = a>= (minThree a b c) || b >= (minThree a b c) || c>= (minThree a b c)
prop_minThree2 a b c = a == (minThree a b c) || b == (minThree a b c) || c == (minThree a b c)

capitalize :: Char -> Char

capitalize c
    |isLower c = toUpper c
    |otherwise = c


charToNum :: Char -> Int

charToNum c
    |isDigit c = fromEnum c - fromEnum '0'
    |otherwise = 0

onThreeLines :: String -> String -> String -> String

onThreeLines a b c = a ++ "\n" ++ b ++ "\n" ++ c

romanDigit :: Char -> String

romanDigit a
    | (charToNum a > 0) && (charToNum a <=3) = take (charToNum a) (repeat 'I')
    | (charToNum a == 4) = "IV"
    | (charToNum a == 5) = "V"
    | (charToNum a > 5) && (charToNum a <= 8) = "V"++(take((charToNum a)-5) (repeat 'I'))
    | (charToNum a == 9) = "IX"




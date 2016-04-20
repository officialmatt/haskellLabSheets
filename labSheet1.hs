import Data.Char 
import Data.List

square :: Int -> Int 
square x  = x*x

pyth :: Int -> Int -> Int
pyth x y = square x + square y 

isTripple :: Int -> Int -> Int -> Bool 
isTripple a b c 	
	| pyth a b == square c = True
	| otherwise = False

isTrippleAny :: Int -> Int -> Int -> Bool
isTrippleAny a b c = isTripple a b c || isTripple c b a || isTripple a c b 

halfEvens :: [Int] -> [Int]
halfEvens xs = [if mod x 2 == 0 then div x 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange y z xs = [x | x <- xs , x >= y, x <= z]

countPositives :: [Int] -> Int
countPositives xs = length [x | x<-xs, x>0]

capitalised :: String -> String
capitalised (x:xs) = [if x==y then toUpper y else toLower y | y<-x:xs ]

capitalisedRest :: String -> String
capitalisedRest [] = []
capitalisedRest xs = if length xs >3 then capitalised xs else lowerCase xs

lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = toLower x:lowerCase xs

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalised x:[capitalisedRest y | y<-xs]

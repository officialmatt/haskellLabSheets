import Data.Char

inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange y z (x:xs)
	| x>=y && x<=z = x:inRange y z xs
	| otherwise = inRange y z xs


countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs)
	| x >0 = 1 + countPositives xs
	| otherwise = countPositives xs

lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = toLower x: lowerCase xs

capitalised :: String -> String 
capitalised [] = []
capitalised (x:xs) = toUpper x:lowerCase xs

--Functions for Title 
checkLength :: String -> String 
checkLength xs
	| length xs > 3 = capitalised xs
	| otherwise = lowerCase xs

checkRest :: [String] -> [String] 
checkRest [] = []
checkRest (x:xs) = checkLength x:checkRest xs

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalised x: checkRest xs

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) = if x <=y then x:(y:ys) else y:insert' x ys


isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
	| x < y = x:merge xs (y:ys)
	| otherwise = y:merge (x:xs) ys

split' :: [a] -> ([a],[a])
split' xs = (take n xs, drop n xs)
	where n = (length xs) `div` 2 

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ls) (msort rs)
	where (ls,rs) = split' xs



rotor :: Int -> [Char] -> [Char] 
rotor 0 xs = xs
rotor _ [] = []
rotor z (x:xs)
	| z < 0 = "Error - the number must be greater than 0"
	| z>= length xs = "Error - the number can't be bigger than the list"
	| otherwise = rotor (z-1) (xs ++ [x])


makeKey :: Int -> [(Char,Char)]
makeKey x = zip ['A'..'Z'] (rotor x ['A'..'Z'])

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c [] = c
lookUp c ((x,y):zs) 
	| c==x = y 
	| otherwise = lookUp c zs

encipher :: Int -> Char -> Char
encipher z c = lookUp c (makeKey z)

normalise :: String -> String
normalise xs = [toUpper x | x <- xs, x `elem` ['A'..'z'] || x `elem` ['0'..'9']]

completeFunction :: Int -> String -> String 
completeFunction c (x:xs) = encipher c x:encipherStr c xs

encipherStr :: Int -> String -> String
encipherStr _ [] = []
encipherStr c (xs) = completeFunction c (normalise xs)

	













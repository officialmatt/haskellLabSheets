mult :: [Int] -> Int
mult [] = 0
mult [x] = x
mult (xs) = foldr (\x acc -> acc * x) 1 xs

posList :: [Int] -> [Int]
posList xs = filter (>0) xs

trueList :: [Bool] -> Bool
trueList xs
	|  length (filter (==False) xs) >0 = False
	|  otherwise = True

evenList :: [Int] -> Bool
evenList xs
	| length (filter odd xs) > 0   = False
	| otherwise = True

maxList :: (Ord a) => [a] -> a
maxList  = foldr1 (\x acc -> if x > acc then x else acc)

inRange :: Int -> Int -> [Int] -> [Int]
inRange x y zs = filter (<=y) (filter (>=x) zs)

countPositives :: [Int] -> Int
countPositives xs = length (filter (>0) xs)

myLength :: [a] -> Int
myLength xs = foldr (\x acc -> acc + x) 0 (map (\x -> 1) xs)

myMap :: (a->b) -> [a] -> [b]
myMap f xs = foldr (\x acc -> f x: acc) [] xs

myLength' :: [a] -> Int
myLength' xs = foldr (\x acc -> acc + 1) 0 (foldr (\x acc -> 1:acc) [] xs)


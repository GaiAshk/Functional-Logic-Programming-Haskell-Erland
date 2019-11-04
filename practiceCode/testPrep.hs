import Data.Char

prefix :: [Int] -> [[Int]]
prefix [] = [[]]
prefix xs = prefix' xs (length(xs))

range :: Int -> [Int]
range 0 = []
range i = 1 : (map succ (range (i-1)))


prefix' :: [Int] -> Int -> [[Int]]
prefix' [] i = []
prefix' xs 0 = []
prefix' xs i = prefix' xs (i-1) ++ [take i xs]

prefixes :: [a] -> [[a]]
prefixes xs = map (\n -> take n xs) (range (length xs))


rearrange :: [Int] -> [a] -> [a]
--rearrange [] str = []
--rearrange (x:xs) str = (str !! x) : rearrange xs str
rearrange xs str = map ((!!) str) xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = []
transpose xs = map head xs : transpose (map tail xs)

-- TREES --

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node left right) = 1 + max (height left) (height right)

data Direction = L | R deriving(Show)
type Path = [Direction]

-- Functions from Notes -- 

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length2 :: [a] -> Int
length2 = foldr (flip $ const . (+1)) 0

length3 :: [a] -> Int
length3 = foldl (\ n _ -> n+1) 0

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum xs)

sum2 :: [Int] -> Int
sum2 = foldr (+) 0

myLast :: [a] -> a
myLast = head . reverse

arr = [0,1,2,3,4,5,6,7,8]

product2 :: [Int] -> Int
product2 = foldl (*) 1

minimum2 :: [Int] -> Int
minimum2 xs = foldr (\ x r -> if (x < r) then x else r) (head xs) xs

maximum2 :: [Int] -> Int
maximum2 (x:xs) = foldl (\ x r -> if (x < r) then r else x) x xs

reverse2 :: [a] -> [a]
reverse2 = foldr (\ x r -> r ++ [x]) []

ee :: [a] -> Int -> a
ee xs 0 = head xs
ee (x:xs) i = ee xs (i-1)

last2 :: [Int] -> Int
last2 = foldl (flip const) 1 

last3 :: [a] -> a
last3 (x:xs) = foldl (\r x -> x) x xs

last4 :: [Int] -> Int
last4 = foldr (\x r -> if (r == 0) then x else r) 0

-- tomer not working properly
init2 :: [a] -> [a]
init2 xs = foldl (\acc curr -> if ((length acc) == (length xs - 1)) then acc else acc ++ [curr]) [] xs

init3 :: [a] -> [a]
init3 xs = take (length xs - 1) xs

init4 :: [a] -> [a]
init4 xs = foldl (\r x -> if (length r == length xs - 1) then r else r ++ [x]) [] xs

init5 xs = foldl (\base item -> base ++ [(xs !! item)]) [] [0..(length xs -2)]

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ (concat2 xs)

concat3 :: [[a]] -> [a]
concat3 = foldr (++) []

concat4 :: [[a]] -> [a]
concat4 = concat_tail []

concat5 :: [[a]] -> [a]
concat5 = foldl (\r x -> r ++ x) []

concat_tail :: [a] -> [[a]] -> [a]
concat_tail acc [] = acc
concat_tail acc (x:xs) = concat_tail (acc ++ x) xs

take2 :: Int -> [a] -> [a]
take2 i = foldl (\r x -> if (length r == i) then r else r ++ [x]) []

drop2 :: Int -> [a] -> [a]
drop2 i xs = foldr (\x r -> if ((length r) == (length xs - i)) then r else (x:r)) [] xs

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 a = foldr (\x r -> r || (a == x)) False


slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = [] 
slice 0 0 (x:xs) = [x] 
slice 0 m (x:xs) = x : slice 0 (m - 1) xs 
slice n m (x:xs) = slice (n - 1) (m - 1) xs

slice' :: Int -> Int -> [a] -> [a]
slice' n m xs = slice2 n (m+1) xs [] 

slice2 :: Int -> Int -> [a] -> [a] -> [a]
slice2 _ _ [] ys = ys 
slice2 0 0 xs ys = ys 
slice2 0 m (x:xs) ys = x : slice2 0 (m - 1) xs ys 
slice2 n m (x:xs) ys = slice2 (n - 1) (m - 1) xs ys

(!!!) :: [Int] -> Int -> Int
(!!!) xs i = head ( drop i xs )

range2 :: Int -> [Int]
range2 0 = []
range2 i = 1 : map (+1) (range2 (i-1))

scan :: [Int] -> [Int]
scan xs = foldl (\r y -> if (null r) then [y] else if ((head r) > y) then y : (head r) : (tail r) else (y:r)) [] xs

sumOfDifs1 :: String -> Int
sumOfDifs1 "" = 0
sumOfDifs1 (x:xs) = digitToInt x + sumOfDifs1 xs

sumOfDifs2 :: [Char] -> Int
sumOfDifs2 = sum . map digitToInt






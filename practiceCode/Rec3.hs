length2 :: [a] -> Int
length2 [] = 0
length2 xs = len2 xs 0 

len2 :: [a] -> Int -> Int
len2 [] acc = acc + 0
len2 (x:xs) acc = len2 xs (acc + 1)

length3 :: [a] -> Int
length3 [] = 0
length3 (x:xs) = length3 xs + 1

reverse2ndHalf :: [a] -> [a]
reverse2ndHalf [] = []
reverse2ndHalf [x] = [x]
reverse2ndHalf xs = reverse2ndHalf2 ( splitAt (div (length2 xs) 2) xs )

reverse2ndHalf2 :: ([a], [a]) -> [a]
reverse2ndHalf2 (xs, ys) = concat [xs , reverse ys]

areEqualList :: [Int] -> [Int] -> Bool
areEqualList [] [] = True
areEqualList [] _ = False
areEqualList _ [] = False
areEqualList (x:xs) (y:ys) = (x == y) && (areEqualList xs ys)

pascal :: Int -> [Int]
pascal 0 = [1]
pascal 1 = [1, 1]
pascal i = [1] ++ (nextPascal (pascal (i-1))) ++ [1]

nextPascal :: [Int] -> [Int]
nextPascal (x : (y : [])) = [x + y]
nextPascal (x : (y : xs)) = (x + y) : (nextPascal (y : xs))

parentheses :: String -> Bool
parentheses [] = True
parentheses str = parentheses' 0 0 str 

parentheses' :: Int -> Int -> String -> Bool
parentheses' i j "" = (i == j)
parentheses' i j (s:str) = if (s == '(') then parentheses' (i+1) j str
                           else if (s == ')') then parentheses' i (j+1) str
                           else parentheses' i j str

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 [a] = [a]
reverse2 xs = reverse2' [] xs

reverse2' :: [a] -> [a] -> [a]
reverse2' acc [] = acc
reverse2' acc (x:xs) = reverse2' (x:acc) xs

cons :: [a] -> [a] -> [a]
cons xs ys = foldr (:) ys xs 








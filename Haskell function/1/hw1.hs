
--Gai Ashkenazy
--204459127 

-- HW 1 

--Question 1 

isPali :: String -> Bool
isPali (xs) = isPali2 xs (reverse xs)


isPali2 :: String -> String -> Bool
isPali2 [] [] = True
isPali2 [a] [] = False
isPali2 [] [a] = False
isPali2 (x:xs) (y:ys) = (x==y) && (isPali2 xs ys)


--Question 2 

prefix :: String -> String -> Bool
prefix [] xs = True
prefix xs [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys


--Question 3 

--a
doubleList :: Int -> [Int]
doubleList 0 = [0]
doubleList n = ( (2*n) : doubleList (n-1) )  


--b 
listDouble :: Int -> [Int]
listDouble 0 = [0]
listDouble n = (2*n) : (listDouble (n-1))


--Question 4

myGcd :: Int -> Int -> Int
myGcd 0 b = b
myGcd a 0 = a
myGcd a b = if (a == b) then a
            else if (a > b) then (myGcd b ( mod a b))
            else (myGcd b a)


-- Question 5

--a 
toDigits :: Integer -> [Integer]
toDigits n = if (n < 1) then []
             else (toDigits (n `div` 10)) ++ ([n `mod` 10])

--b
doubleIntermittent :: [Integer] -> [Integer]
doubleIntermittent [] = []
doubleIntermittent [a] = [a]
doubleIntermittent (x:y:xs) = (2*x) : y : (doubleIntermittent xs) 

--c
sumDigitsInList :: [Integer] -> Integer
sumDigitsInList xs = sumDigitsInList_acc xs 0

sumDigitsInList_acc :: [Integer] -> Integer -> Integer
sumDigitsInList_acc [] acc = acc
sumDigitsInList_acc (x:xs) acc = sumDigitsInList_acc xs (acc + (sum (toDigits x)))

--d
validate :: Integer -> Bool 
validate xs = ((sumDigitsInList (doubleIntermittent (toDigits xs))) `mod` 10) == 0


-- Question 6

--a
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + (mySum xs) 

--b
mySumTail :: [Int] -> Int
mySumTail xs = mySumTail' xs 0

mySumTail' :: [Int] -> Int -> Int  
mySumTail' [] acc = acc
mySumTail' (x:xs) acc = mySumTail' xs (acc + x)


--Question 7 
perfect :: Integer -> Bool
perfect num = perfect' num (num `div` 2) 0

perfect' :: Integer -> Integer -> Integer -> Bool
perfect' num 0 acc = (num == acc)
perfect' num r acc = if((num `mod` r) == 0) then perfect' num (r-1) (acc + r)
                     else perfect' num (r-1) acc



-------- test ---------

isFali :: String -> Bool
isFali "" = True
isFali (s:str) = let y = last(s:str)
                 in (s == y) && isFali(init2 str)
				 
init2 :: [a] -> [a]
init2 [] = []	
init2 xs = init xs




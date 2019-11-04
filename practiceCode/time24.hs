import Data.Char (intToDigit)
-- intToDigit converts a single Int digit into the corresponding digit Char

time24 :: Int -> (Int, Int, Int)
time24 (s) = (s `div` 3600, (s `mod` 3600) `div` 60, (s `mod` 3600) `mod` 60)

power2 :: Int -> Int
power2 0 = 1
power2 n = 2 * power2 (n-1)


fact3 :: Int -> Int
fact3 n 
   | n == 0    =1
   | otherwise = n * fact3 (n-1)


loan = 1000000
interest = 0.03
monthlyReturn = 5545
   
debt :: Int -> Float
debt 0 = loan
debt 1 = loan + loan*(interest/12) - monthlyReturn
debt n = let prevDebt = debt (n-1) 
         in 
         prevDebt + prevDebt*(interest/12) - monthlyReturn
-- debt n = debt(n-1) + debt(n-1)*(interest/12) - monthlyReturn


addPoints :: (Float, Float) -> (Float, Float) -> (Float, Float)
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


unBox :: (Int, Int, Int) -> [Int]
unBox (x, y, z) = [x,y,z]

-- length of a list
ilength :: [Int] -> Int
ilength [] = 0
ilength (x:xs) = 1 + ilength xs 

-- sum of a list 
sum_list :: [Int] -> Int
sum_list [] = 0
sum_list (x:xs) = x + sum_list xs 


-- max of a list 
max_list :: [Int] -> Int
max_list [] = 0
max_list (x:xs) = max x (max_list xs) 

-- max of a list, tail recursion 
max_list_acc :: [Int] -> Int -> Int
max_list_acc [] acc = acc
max_list_acc (x:xs) acc = if ( x > acc) 
                          then max_list_acc xs x 
                          else max_list_acc xs acc

-- concatanate two lists 
nconcat :: [Int] -> [Int] -> [Int]
nconcat [] ls = ls
nconcat (x:xs) ls = x : nconcat xs ls

-- intToDigit converts a single Int digit into the corresponding digit Char

--num_to_str :: Integer -> String
--num_to_str 0 = ""
--num_to_str n = num_to_str (n `div` 10) ++ [intToDigit (n `mod` 10)]





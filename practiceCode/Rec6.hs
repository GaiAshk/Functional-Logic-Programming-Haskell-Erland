squares :: Int -> [Int]
squares n = map(\x -> x*x) [1..n]

--add : : Int -> Int -> Int
--add x y = x + y

add :: Int -> Int -> Int
add = \x y -> x+y

sum2 :: [Int] -> Int
sum2 = foldr (+) 0

--concat : : [ [ a ] ] -> [ a ]
--concat [ ] = [ ]
--concat ( x :xs) = x ++ concat xs

concat2 :: [[a]] -> [a]
concat2 = foldr (++) []

--(++) : : [ a ] -> [ a ] -> [ a ]
--(++) [ ] ys = ys
--(++) ( x :xs) ys = x : ( (++) xs ys)  

concat3 :: [a] -> [a] -> [a]
concat3 xs ys = foldr (:) ys xs

 

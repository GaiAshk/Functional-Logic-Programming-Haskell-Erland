avg :: Int -> Int -> Int
avg a b = (a + b) `div` 2

factored2or3 :: Int -> Bool
factored2or3 x = ((mod x 2) == 0) || ((mod x 3) == 0)

mult :: Int -> Int -> Int
mult 0 _ = 0
mult _ 0 = 0
mult 1 x = x
mult y x = (mult (y-1) x) + x

first, second, third :: (Int ,Int ,Int ) -> Int
first ( x , _ , _) = x
second ( _ , y, _) = y
third ( _ , _ , z) = z

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-- Gai Ashkenazy
-- 204459127

-- HW 2

-- Question 1 a
myCurry:: ((t1, t2) -> t) -> (t1 -> t2 -> t)
myCurry f x y = f (x, y)

-- b
myUncurry :: (t1 -> t2 -> t) -> (t1, t2) -> t
myUncurry f (x, y) = f x y

-- Question 2
bundler :: Eq a => [a] -> [[a]]
bundler [] = []
bundler[a] = [[a]]
bundler xs = let count = countInRow xs
             in [take count xs] ++ bundler (drop count xs)

countInRow :: Eq a => [a] -> Int
countInRow [] = 0
countInRow [a] = 1
countInRow (x:y:xs) = if (x == y) then 1 + (countInRow (y:xs))
                      else 1

-- Question 3
numAppearances :: Eq t => [t] -> [(Int, t)]
numAppearances [] = []
numAppearances xs = let list = (bundler xs)
                    in  zip (map length list) (map head list)

-- Question 4
dropMod:: [a] -> Int -> [a]
dropMod [] _ = []
dropMod xs n = dropMod' xs n 1

dropMod' :: [a] -> Int -> Int -> [a]
dropMod' [] _ _ = []
dropMod' (x:xs) n i = if (n == i) then dropMod' xs n 1
                      else x : dropMod' xs n (i+1)

-- Question 5
toBinary:: Integral a => a -> [a]
toBinary 0 = [0]
toBinary 1 = [0, 1]
toBinary n = toBinary (n `div` 2) ++ [(n `mod` 2)]

-- Question 6 
-- a 
insertItem :: (String, a) -> [(String,a)] -> [(String,a)]
insertItem x dataBase = x : dataBase 

-- b 
itemsByKey :: String -> [(String, a)] -> [a]
itemsByKey str [] = []
itemsByKey str ((key, item):dataBase) = if (str == key) then (item : itemsByKey str dataBase)
                                        else (itemsByKey str dataBase)

-- c 
getKeys :: [(String, a)] -> [String]
getKeys [] = []
getKeys ((key, item) : dataBase) = key : filter (/= key) (getKeys dataBase)

-- d 
groupItemsByKey:: [(String, a)] -> [(String, [a])]
groupItemsByKey [] = []
groupItemsByKey xs = groupItemsByKey' (getKeys xs) xs 

groupItemsByKey' :: [String] -> [(String,a)] -> [(String,[a])]
groupItemsByKey' [] _ = []
groupItemsByKey' (x:xs) dataBase =  (x,(itemsByKey x dataBase)) : (groupItemsByKey' xs dataBase)

-- Question 7
bar :: (a -> (b -> c) -> c) -> ((a -> c) -> c) -> (b -> c) -> c
bar f g h = g (flip f h)

foo :: (a -> (b, c)) -> (b -> d) -> (c -> d -> e) -> a -> e
foo f g h x = h (snd (f x)) (g (fst (f x)))



















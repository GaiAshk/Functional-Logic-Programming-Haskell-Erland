--312275431
--Maayan Wagenheim

--HW2

--q1 a
myCurry :: ((t1, t2) -> t) -> t1 -> t2 -> t
myCurry f x1 x2 = f (x1,x2)

--q1 b
myUncurry :: (t1 -> t2 -> t) -> (t1, t2) -> t
myUncurry f (x1,x2) = f x1 x2

--q2
bundler :: Eq a => [a] -> [[a]]
bundler [] = []
bundler [x] = [[x]]
bundler xs = let count = countConsecutive xs 1
             in [take count xs] ++ bundler (drop count xs)

countConsecutive :: Eq a => [a] -> Int -> Int
countConsecutive [] acc = acc
countConsecutive (x:xs) acc =  if (x == head xs) then countConsecutive xs (acc+1)
                               else  acc

--q3
numAppearances :: Eq t => [t] -> [(Int, t)]
numAppearances [] = []
numAppearances xs = let list = (bundler xs)
                    in  zip (map length list) (map head list)

--q4
dropMod :: [a] -> Int -> [a]
dropMod [] _ = []
dropMod xs n = dropModAcc xs n 1

dropModAcc :: [a] -> Int -> Int -> [a]
dropModAcc [] _ _ = []
dropModAcc (x:xs) n i = if (n == i) then dropModAcc xs n 1
                        else x : dropModAcc xs n (i+1)

--q5
toBinary :: Integral a => a -> [a]
toBinary 0 = [0]
toBinary x = toBinary (x `div` 2) ++ [x `mod` 2]

--q6 a
insertItem :: (String, a) -> [(String,a)] -> [(String,a)]
insertItem t xs = t : xs

--q6 b
itemsByKey :: String -> [(String,a)] -> [a]
itemsByKey str [] = []
itemsByKey str ((key,item):ts) = if (key == str) then (item : itemsByKey str ts)
                                 else (itemsByKey str ts)

--q6 c
getKeys :: [(String,a)] -> [String]
getKeys [] = []
getKeys ((t1,t2):ts) = t1 : filter (/= t1) (getKeys ts)

--q6 d
groupItemsByKey :: [(String,a)] -> [(String,[a])]
groupItemsByKey [] = []
groupItemsByKey xs = groupItemsByKeyi (getKeys xs) xs 

groupItemsByKeyi :: [String] -> [(String,a)] -> [(String,[a])]
groupItemsByKeyi [] _ = []
groupItemsByKeyi (x:xs) ts =  (x,(itemsByKey x ts)) : (groupItemsByKeyi xs ts)

--q7
bar :: (a -> (b -> c) -> c) -> ((a -> c) -> c) -> (b -> c) -> c
bar f g h = g (flip f h)

foo :: (a -> (b, c)) -> (b -> d) -> (c -> d -> e) -> a -> e
foo f g h x = h (snd (f x)) (g (fst (f x)))

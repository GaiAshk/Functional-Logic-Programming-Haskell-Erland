apply :: (a -> b) -> a -> b
apply f x = f x

sumOfSquares :: [Int] -> Int
sumOfSquares [] = 0
sumOfSquares xs = sum (map square xs)

square :: Int -> Int
square x = x * x 

range :: Int -> [Int] 
range 0 = []
range i = 1 : (map (+1) (range (i-1)))

prefixes :: [a] -> [[a]]
prefixes xs = let prefix n = take n xs
               in map prefix (range (length xs))

partialSums :: [Int] -> [Int]
partialSums xs = map sum (prefixes xs)

rearrange :: [Int] -> [a] -> [a]
rearrange [] _ = []
rearrange _ [] = []
rearrange (i:index) xs = ((!!) xs i) : rearrange index xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = []
transpose xss = map head xss : transpose (map tail xss)


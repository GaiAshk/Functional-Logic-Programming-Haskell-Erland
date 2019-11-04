-- Gai Ashkenazy
-- 204459127

-- HW 3

import Data.Char

-- Question 1

-- a
byteSub :: Char -> Char
byteSub 'a' = 'h'
byteSub 'g' = 'b'
byteSub 'z' = 'a'
byteSub a = if (ord a < ord 'a' || ord a > ord 'z') then '0'
            else chr ( (ord a) + 1)

-- b
shiftIn :: Int -> [Char]->[Char]
shiftIn 0 xs = xs
shiftIn _ [] = []
shiftIn x xs = shiftIn (x-1) ((last xs) : (init xs))

-- c
shiftRows :: [[Char]]->[[Char]]
shiftRows [[]] = [[]]
shiftRows xs = shiftRows' 0 xs 

shiftRows' :: Int -> [[Char]] -> [[Char]]
shiftRows' i [] = []
shiftRows' i (x:xs) = (shiftIn i x) : (shiftRows' (i+1) xs)  

-- d 
roundKey :: (Char->Char)->[[Char]]->[[Char]]
roundKey f xs = map (map f) xs

-- e 
simpleAES :: (Char->Char)->[[Char]]->String
simpleAES f xs = listToString (roundKey f (shiftRows xs))

listToString :: [[Char]] -> String
listToString xs = foldr (++) "" xs


-- Question 2

data JVal = JStr String | JNum Double | JBool Bool |JNull | JObj [(String, JVal)] | JArray [JVal]

jsonToString :: JVal -> String
jsonToString JNull = "null"
jsonToString (JStr x) = x 
jsonToString (JBool True) = "true"
jsonToString (JBool False) = "false"
jsonToString (JNum x) = show x
jsonToString (JArray []) = "[]"
jsonToString (JObj []) = "{}"
jsonToString (JArray arr) = let n = insertComma (length(arr) - 1) arr
                            in concat (["["] ++ ( map jsonToString n) ++ ["]"])
jsonToString (JObj xs) = "{" ++ (makeObj ((length xs) - 1) xs) ++ "}"
 
 
insertComma :: Int -> [JVal] -> [JVal]
insertComma 0 a = a
insertComma i (a:arr) = [a] ++ [JStr ","] ++ insertComma (i-1) arr

makeObj :: Int -> [(String, JVal)] -> String
makeObj _ [] = []
makeObj 0 [(t1, t2)] = concat ([t1] ++ [": "] ++ [jsonToString t2])
makeObj i ((t1, t2) : xs) = concat ([t1] ++ [": "] ++ [jsonToString t2] ++ [", "] ++ [(makeObj (i-1) xs)] )

-- Question 3 

-- a
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- b 
myMap:: (a->b) -> [a]->[b]
myMap f = foldr (\x acc -> (f x) : acc) []  

-- c 
myNegate:: [Int] -> [Int]
myNegate xs = map negate (map abs xs)


-- Question 4


setElement :: Int -> a -> [a] -> [a]
setElement n x xs = if ((n < length xs) && n >= 0 ) then (take n xs) ++ [x] ++ (drop (n+1) xs)
                    else xs

setElements :: [(Int, a)] -> [a] -> [a]
setElements xs ys =  foldl (flip (uncurry setElement)) ys xs

setElements2 :: [(Int, a)] -> [a] -> [a]
setElements2 xs ys = foldl (\r (x,c) -> setElement x c r) ys xs 


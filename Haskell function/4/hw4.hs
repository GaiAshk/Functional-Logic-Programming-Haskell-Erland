-- Gai Ashkenazy
-- 204459127

-- HW 4

-- Question 1
-- a
naturals :: [Integer]
naturals = 1 : map (+1) naturals

-- b 
squares :: [Integer]
squares = map (^2) naturals

-- c
threes :: [Integer]
threes = map (*3) naturals

-- d 
res :: [Integer]
res = res' naturals squares threes

res' :: [Integer] -> [Integer] -> [Integer] -> [Integer] 
res' (n:nats) (s:squs) (t:thrs) = [n, s, t] ++ res' nats squs thrs 

-- e
switch :: [a] -> [a]
switch (x:y:xs) = y : x : (switch xs)


-- Question 2

data BinaryTree a = Nil | BNode a (BinaryTree a) (BinaryTree a) deriving (Show)

-- a 
infTree :: a -> BinaryTree a
infTree x = BNode x (infTree x) (infTree x)

-- b
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Nil = Nil 
treeMap f (BNode a left right) = BNode (f a) (treeMap f left) (treeMap f right) 

-- c 
type Depth = Int 

treeTake :: Depth -> BinaryTree a -> BinaryTree a
treeTake 0 _ = Nil 
treeTake i Nil = Nil
treeTake i (BNode t left right) = BNode t (treeTake (i-1) left) (treeTake (i-1) right)  

-- d
treeSort :: BinaryTree t -> [t]
treeSort Nil = []
treeSort (BNode x left right) = (treeSort left) ++ (x : (treeSort right))





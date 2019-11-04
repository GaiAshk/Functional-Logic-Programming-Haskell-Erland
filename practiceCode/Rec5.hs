transpose :: [[a]] -> [[a]]
transpose ([]:xs) = []
transpose xs = (map head xs) : transpose (map tail xs)

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf l) = 0
height (Node a b) = max (height a) (height b) +1

data Direction = L | R deriving (Show)

type Path = [Direction]

treeExample :: Tree Int
treeExample = Node
                (Node 
                    (Leaf 1)
					(Node 
					    (Leaf 2)
						(Leaf 3)
					)
                )
                (Node 
                    (Leaf 4)
                    (Node 
                        (Leaf 5)
                        (Leaf 6)
                    )
                )

paths :: Tree a -> [(a, Path)]
paths (Leaf x) = [(x, [])]
paths (Node l r) = addDirection L (paths l) ++ addDirection R (paths r)


addDirection :: Direction -> [(a, Path)] -> [(a, Path)]
addDirection d [] = []
addDirection d ((x, p) : ps) = (x, d:p) : addDirection d ps


search :: Int -> [(Int, Path)] -> Path
search _ [] = []
search x ((y,p) : xs) = if (x == y) then p
                        else search x xs

getPath :: Int -> Tree Int -> Path
getPath x t = search x (paths t)






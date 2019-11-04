-- Gai Ashkenazy
-- 204459127

-- Maayan Wag
-- 312275431

-- HW 7

-- Question 1

type Color = Int
type Index = (Int,Int)
type Cells = [(Index,Color)]

-- converts from Index to Int
indexToInt :: Index -> Int
indexToInt (i, j) = (i * 8) + (j)

-- converts from Int to Index
intToIndex :: Int -> Index
intToIndex i = (i `div` 8 , i `mod` 8)

-- returns the color of a cell in the matrix
getColor :: Cells -> Index -> Color
getColor cell i = snd (cell !! (indexToInt i))

-- addes the cell on the Right to the current best list only if the index is valid and the colors are the same
addRight :: Cells -> Index -> [Index]
addRight cell (i, j) = let c = getColor cell (i, j)
                       in if (j < 7 && c == (getColor cell (i, j+1))) then [(i, j+1)]
                          else []

-- addes the cell on the Down to the current best list only if the index is valid and the colors are the same
addDown :: Cells -> Index -> [Index]
addDown cell (i, j) = let c = getColor cell (i, j)
                      in if (i < 5 && c == (getColor cell (i+1, j))) then [(i+1, j)]
                         else []


-- addes the cell on the UP to the current best list only if the index is valid and the colors are the same
addUp :: Cells -> Index -> [Index]
addUp cell (i, j) = let c = getColor cell (i, j)
                      in if (i > 0 && c == (getColor cell (i-1, j))) then [(i-1, j)]
                         else []

-- addes all the indexes next to this one that are in the same color and in range
addAll :: Cells -> Index -> [Index]
addAll cell i = [i] ++ addUp cell i ++ addDown cell i ++ addRight cell i


-- direction 1 means we just went down 
-- direction 0 mean we just went up
-- finds the CC from this index, returns a list of indexes that can be with duplicates
findCC :: Cells -> Index -> Color -> Int -> [Index]
findCC cell (6, j) c direction =  []
findCC cell (i, 8) c direction =  []
findCC cell (i, -1) c direction =  []
findCC cell (-1, j) c direction =  []
findCC cell (i, j) c d = if (c /= getColor cell (i, j)) then []
                       else if (d == 0) then addAll cell (i,j) ++ findCC cell (i+1, j) c 2 ++ findCC cell (i,j+1) c 0
                       else if (d == 1) then addAll cell (i,j) ++ findCC cell (i+1, j) c 2 ++ findCC cell (i, j-1) c 1 
                       else addAll cell (i,j) ++ findCC cell (i+1, j) c 2 ++ findCC cell (i,j+1) c 0 ++ findCC cell (i, j-1) c 1

-- removes the duplicates from a list of indexes
removeDuplicates :: [Index] -> [Index] -> [Index]
removeDuplicates [] acc = reverse acc
removeDuplicates ((i, j):[]) acc = reverse ((i, j):acc)
removeDuplicates ((i, j):xs) acc = if( elem (i,j) xs) then removeDuplicates xs acc
                                   else removeDuplicates xs ((i,j):acc)

--run on all the cells in the matrix and returns the LCC found in all the iterations
runOnCells :: Cells -> Index -> [Index] ->[Index]
runOnCells example (5,7) best = best
runOnCells example (i,j) best = let current = removeDuplicates (findCC example (i,j) (getColor example (i,j)) 2) []
                                    a = length(current)
                                    b = length(best) 
                                in if (a > b) then runOnCells example (intToIndex (indexToInt (i,j) + 1)) current
                                   else runOnCells example (intToIndex (indexToInt (i,j) + 1)) best

--final command to find LCC on the given size matrix
getLCC :: Cells -> [Index]
getLCC example = runOnCells example (0,0) []



-- tests --

example :: [((Int, Int), Int)]
example = [((0,0),1),((0,1),4),((0,2),4),((0,3),4),((0,4),4),((0,5),3),((0,6),3),((0,7),1),
           ((1,0),2),((1,1),1),((1,2),1),((1,3),4),((1,4),3),((1,5),3),((1,6),1),((1,7),1),
           ((2,0),3),((2,1),2),((2,2),1),((2,3),1),((2,4),2),((2,5),3),((2,6),2),((2,7),1),
           ((3,0),3),((3,1),3),((3,2),2),((3,3),1),((3,4),2),((3,5),2),((3,6),2),((3,7),2),
           ((4,0),3),((4,1),1),((4,2),3),((4,3),1),((4,4),1),((4,5),4),((4,6),4),((4,7),4),
           ((5,0),1),((5,1),1),((5,2),3),((5,3),1),((5,4),1),((5,5),4),((5,6),4),((5,7),4)]


example2 :: [((Int, Int), Int)]
example2 = [((0,0),1),((0,1),4),((0,2),4),((0,3),4),((0,4),4),((0,5),3),((0,6),3),((0,7),1),
           ((1,0),2),((1,1),1),((1,2),1),((1,3),4),((1,4),3),((1,5),3),((1,6),1),((1,7),1),
           ((2,0),3),((2,1),2),((2,2),1),((2,3),1),((2,4),2),((2,5),3),((2,6),2),((2,7),1),
           ((3,0),3),((3,1),3),((3,2),2),((3,3),2),((3,4),2),((3,5),2),((3,6),2),((3,7),2),
           ((4,0),3),((4,1),1),((4,2),3),((4,3),1),((4,4),1),((4,5),4),((4,6),4),((4,7),4),
           ((5,0),1),((5,1),1),((5,2),3),((5,3),1),((5,4),1),((5,5),4),((5,6),4),((5,7),4)]


example3 :: [((Int, Int), Int)]
example3 = [((0,0),1),((0,1),4),((0,2),4),((0,3),4),((0,4),4),((0,5),3),((0,6),3),((0,7),4),
           ((1,0),2),((1,1),1),((1,2),1),((1,3),4),((1,4),3),((1,5),3),((1,6),1),((1,7),4),
           ((2,0),3),((2,1),2),((2,2),1),((2,3),1),((2,4),2),((2,5),3),((2,6),2),((2,7),4),
           ((3,0),3),((3,1),3),((3,2),2),((3,3),2),((3,4),2),((3,5),2),((3,6),2),((3,7),4),
           ((4,0),3),((4,1),1),((4,2),3),((4,3),1),((4,4),1),((4,5),4),((4,6),4),((4,7),4),
           ((5,0),1),((5,1),1),((5,2),3),((5,3),1),((5,4),1),((5,5),4),((5,6),4),((5,7),4)]

example4 :: [((Int, Int), Int)]
example4 = [((0,0),1),((0,1),4),((0,2),4),((0,3),4),((0,4),4),((0,5),3),((0,6),3),((0,7),1),
           ((1,0),2),((1,1),1),((1,2),1),((1,3),4),((1,4),3),((1,5),3),((1,6),1),((1,7),1),
           ((2,0),3),((2,1),2),((2,2),1),((2,3),1),((2,4),2),((2,5),3),((2,6),2),((2,7),1),
           ((3,0),3),((3,1),3),((3,2),2),((3,3),1),((3,4),2),((3,5),2),((3,6),2),((3,7),2),
           ((4,0),3),((4,1),1),((4,2),3),((4,3),1),((4,4),1),((4,5),4),((4,6),4),((4,7),4),
           ((5,0),1),((5,1),1),((5,2),1),((5,3),1),((5,4),1),((5,5),4),((5,6),4),((5,7),4)]

example5 :: [((Int, Int), Int)]
example5 = [((0,0),1),((0,1),1),((0,2),1),((0,3),1),((0,4),1),((0,5),1),((0,6),1),((0,7),1),
           ((1,0),1),((1,1),2),((1,2),4),((1,3),4),((1,4),3),((1,5),3),((1,6),4),((1,7),1),
           ((2,0),1),((2,1),2),((2,2),4),((2,3),3),((2,4),2),((2,5),3),((2,6),2),((2,7),1),
           ((3,0),1),((3,1),3),((3,2),2),((3,3),3),((3,4),2),((3,5),2),((3,6),2),((3,7),1),
           ((4,0),1),((4,1),3),((4,2),3),((4,3),3),((4,4),2),((4,5),4),((4,6),4),((4,7),1),
           ((5,0),1),((5,1),1),((5,2),1),((5,3),1),((5,4),1),((5,5),1),((5,6),1),((5,7),1)]


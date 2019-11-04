hello = "hello"
world = "world!"
helloWorld = hello ++ " " ++ world

rotate :: [a] -> Int -> [a]
rotate xs i = let n = if (i > 0) then i else (length xs) - 1
              in drop n (take (length xs + n) ((++) xs xs))


rotate2 :: [a] -> Int -> [a]
rotate2 xs i = let n = if (i > 0) then i else (mod i (length xs)) + (length xs) - 1
              in drop n (take (length xs + n) (cycle xs))


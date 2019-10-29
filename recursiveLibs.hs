module RecursiveLibs where
    import Data.Bool

    and :: [Bool] -> Bool
    and [False] = False
    and [True] = True
    and (x:xs) | x == False = False
             | otherwise = RecursiveLibs.and xs

    concat :: [[a]] -> [a]
    concat [] = []
    concat (x:xs) = x++(RecursiveLibs.concat xs)

    replicate :: Int -> a -> [a]
    replicate 0 x = []
    replicate n x = x:(RecursiveLibs.replicate (n-1) x)

    sum :: [Int] -> Int
    sum [] = 0
    sum (x:xs) = x + RecursiveLibs.sum xs

    last :: [a] -> a
    last [] = error "Empty list"
    last (x:xs) | length (x:xs) == 1 = x
                | otherwise = RecursiveLibs.last xs
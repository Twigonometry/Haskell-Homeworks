module Pyths where
    pyths :: Int -> [(Int, Int, Int)]
    pyths i = [(x,y,z) | x <- [1..i], y <- [1..i], z <- [1..i], x^2 + y^2 == z^2]
--express [f x | x <- xs, p x] using map and filter
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

--redefine map f and filter p using foldr
foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f = foldr (\x xs -> f x : xs) []

foldrFilter :: (a -> Bool) -> [a] -> [a]
foldrFilter p = foldr (\x ys -> if p x then x:xs else xs) []
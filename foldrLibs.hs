--express [f x | x <- xs, p x] using map and filter
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

--redefine map f and filter p using foldr
{-
foldMap :: (a -> b) -> [a] -> [b]
foldMap f (x:xs) = (f x):(foldr f [] xs)
--foldMap f = foldr f []
-}
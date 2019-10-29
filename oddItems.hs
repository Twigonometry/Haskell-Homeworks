module OddItems where
    oddItemList :: [Int] -> [Int]
    oddItemList xs = filter (\x -> x `mod` 2 /= 0) xs
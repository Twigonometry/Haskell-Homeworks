module GradeBands where
    isInRange :: Ord a => a -> (a, a) -> Bool
    isInRange x (lower, upper) = lower <= x && x <= upper

    --takes list of names and grades, outputs names of people with firsts
    findFirsts :: [(String, Int)] -> [String]
    findFirsts grades = [fst (grade) | grade <- grades, isInRange (snd (grade)) (70, 100)]
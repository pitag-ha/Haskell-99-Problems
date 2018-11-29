rotate :: [Char] -> Int -> [Char]
rotate l n = drop distance l ++ take distance l 
    where distance = n `mod` length l

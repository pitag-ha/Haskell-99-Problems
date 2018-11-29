drop :: [a] -> Int -> [a]
drop l n = [l !! (i-1)  | i <- [0..(length l -  1)], i `mod` n /= 0 ]

mySlice :: [Char] -> Int -> Int -> [Char]
mySlice l s f = [l !! (i-1) | i <- [s..f]]

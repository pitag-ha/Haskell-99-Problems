pack :: (Eq a) => [a] -> [[a]]
pack = foldr accumulate []
    where accumulate elt [] = [[elt]]
          accumulate elt (x:xs) 
                | elt == head x = (elt:x):xs 
                | otherwise = [elt]:x:xs

rlCompress :: (Num n, Eq a) => [[a]] -> [(n, a)]
rlCompress = map (\l -> ((fromIntegral . length) l, head l)) 
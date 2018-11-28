pack :: (Eq a) => [a] -> [[a]]
pack = foldr accumulate []
    where accumulate elt [] = [[elt]]
          accumulate elt (x:xs) 
                | elt == head x = (elt:x):xs 
                | otherwise = [elt]:x:xs
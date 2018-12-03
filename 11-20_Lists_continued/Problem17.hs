mySplit :: Int -> [Char] -> ([Char],[Char])
mySplit n l 
    | length l <= n = error "How did we get here?"
    | otherwise = recHelper n l ([],[])
        where recHelper 0 l' (s,_) = (reverse s, l')
              recHelper n' (x:xs) (s, _) = recHelper (n'-1) xs ((x:s),[])

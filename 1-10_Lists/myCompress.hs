myCompress :: (Eq a) => [a] -> [a]
myCompress = foldr accumulate []
    where accumulate elt [] = [elt]
          accumulate elt acc 
                | elt == head acc = acc
                | otherwise = elt:acc
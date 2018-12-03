dupli :: [a] -> [a]
dupli = concat . map (\e -> [e,e])

repli :: [a] -> Int -> [a]
repli l n = [x | x <- l, _ <- [1..n]]

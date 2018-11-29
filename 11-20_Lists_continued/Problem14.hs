dupli :: [a] -> [a]
dupli = concat . map (\e -> [e,e])

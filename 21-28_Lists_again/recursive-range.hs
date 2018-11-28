recursiveRange :: Int -> Int -> [Int]
recursiveRange s f
    | s == f = [s]
    | otherwise = s:recursiveRange (s+1) f
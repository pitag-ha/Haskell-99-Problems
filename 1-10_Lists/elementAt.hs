elementAt ::(Integral a, Eq l) => [l] -> a -> l
elementAt l n
    | l == [] = error "too big"
    | n < 1 = error "n is too small"
    | n == 1 = head l
    | otherwise = elementAt (tail l) (n - 1)
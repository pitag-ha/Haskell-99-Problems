insert_at :: a -> [a] -> Int -> [a]
insert_at x l n
    | (n > (length l) || n <= 0) = error "Whaat?"
    | n == 1 = x:l
insert_at x (y:ys) n = y:(insert_at x ys (n-1))  
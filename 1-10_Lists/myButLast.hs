myButLast::[a] -> a
myButLast l = if length l >= 2 then l !! (length l - 2) else error "don't know Monads yet :("
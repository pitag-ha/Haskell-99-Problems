myLast :: [a] -> a
myLast [] = error "don't know monads yet :("
myLast (x:[]) = x
myLast (_:xs) = myLast xs
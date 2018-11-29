module EncodeModified(NE(..), safehead) where 

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

pack :: (Eq a) => [a] -> [[a]]
pack = foldr accumulate [] 
    where accumulate elt [] =[[elt]]
          accumulate elt (x:xs)
            | safehead x == Just elt = (elt:x):xs
            | safehead x == Nothing = x:xs
            | otherwise = [elt]:x:xs


data NE = Single Char | Multiple Int Char deriving (Show)

encodeModified :: [Char] -> [NE]
encodeModified = map (\elt ->
     if length elt == 1 then Single $ head elt
     else Multiple (length elt) (head elt)) . pack

encodeModified' :: [Char] -> [NE]
encodeModified' = foldr accumulate [] 
    where accumulate elt [] = [Single elt]
          accumulate elt ((Single x):xs)
            | elt == x = (Multiple 2 elt):xs
            | otherwise = (Single elt):(Single x):xs
          accumulate elt ((Multiple l x):xs) 
            | elt == x = (Multiple (l+1) x):xs
            | otherwise = (Single elt):(Multiple l x):xs

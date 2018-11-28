data NestedList a = Elem a | List [NestedList a]

my_flatten :: NestedList a -> [a]
my_flatten (List []) = []
my_flatten (Elem x) = [x]
my_flatten (List (x:l)) = (my_flatten x) ++ (my_flatten (List l))

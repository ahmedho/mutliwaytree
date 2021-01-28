

-- a parametric data type MultTree a that can be used to represent a  multi way tree
data MultTree a = Null | Node a a [MultTree a] | Leaf a deriving Show

-- a multi way tree as a help method 
t1 :: MultTree Int
t1 = Node 3 42 [ Node 3 15 [Leaf 3, Leaf 11, Leaf 12], Node 19 42 [Leaf 42 , Leaf 23]]

-- This receives a multi-way tree of the MultTree a type and calculates the maximum number of successors a node has in this tree.
verzweigungsgrad :: MultTree a -> Int
verzweigungsgrad Null = 0
verzweigungsgrad (Leaf a) = 0
verzweigungsgrad (Node _ _ []) = 1
verzweigungsgrad (Node _ _ a) = 1 + sum' (map' verzweigungsgrad a)

-- map implementation
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- sum implementation
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- return leaves of a multi way tree as a list
datenListe :: MultTree a -> [a]
datenListe Null = []
datenListe (Leaf a) = a:[]
datenListe (Node _ _ a) = a >>= datenListe

{-This modifies the index nodes so that the smaller of the
two values stored in them corresponds to the smallest value stored in a data node
in that subtree. Similarly, the larger of the two values should correspond
to the largest value in a data node in that subtree.-}
datenIntervalle :: MultTree Int -> MultTree Int
datenIntervalle (Leaf a) = Leaf a
datenIntervalle (Node a b xs) = if a < b then Node (minList (leaf xs) maxBound) (maxList (leaf xs) minBound) (datenIntervalle' xs) else Node (maxList (leaf xs) minBound) (minList (leaf xs) maxBound) (datenIntervalle' xs)

-- help method
datenIntervalle' :: [MultTree Int] -> [MultTree Int]
datenIntervalle' [] = []
datenIntervalle' (t : ts) = datenIntervalle t : datenIntervalle' ts


leaf :: [MultTree a] -> [a]
leaf [] = []
leaf (t:ts) = datenListe t ++ leaf ts

maxList :: [Int] -> Int -> Int
maxList [] i = i
maxList (x : xs) i = if(x > i) then maxList xs x else maxList xs i

minList :: [Int] -> Int -> Int
minList [] i = i
minList (x : xs) i = if(x < i) then minList xs x else minList xs i

-- checks whether the entered value is contained in a tree of the MultTree Int type.
contains :: Int -> MultTree Int -> Bool
contains x (Leaf a) = if a == x then True else False
contains x (Node a b as) = if a > x || x > b then False else (if a == x || b == x then True else contains' x as)
                            
contains' :: Int -> [MultTree Int] -> Bool
contains' x [] = False
contains' x (a:as) = if contains x a then True else contains' x as
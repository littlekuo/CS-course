import Data.List

--ex1
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)


--ex2
data Tree a = Leaf| Node Integer (Tree a) a (Tree a)
                                     deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> insertInTree x tree) Leaf

heightTree :: Tree a -> Integer
heightTree Leaf = -1
heightTree (Node n t1 val t2) = n

insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 Leaf x Leaf
insertInTree x (Node n l val r) 
    | h1 < h2   = Node  n (insertInTree x l) val r 
    | h1 > h2   = Node  n    l val r1 
    | otherwise = Node (h+1) l val r1  
  where h1  = heightTree l
        h2  = heightTree r
        r1 = insertInTree x r
        h   = heightTree r1

--ex3
xor :: [Bool] -> Bool
xor = foldr xor2 False

xor2 :: Bool -> Bool -> Bool
xor2 a b = (a || b) && not (a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base (reverse xs)


--ex4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ sieve
            where sieve = map (\(i, j) -> i + j + 2*i*j) . filter (\(i, j) -> i + j + 2*i*j <= n) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]




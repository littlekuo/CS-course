{-# OPTIONS_GHC -Wall #-}

module Golf where


getL :: Int -> [a] -> [a]
getL n lst = [lst !! i | i <- [n-1, n-1+n..(length lst - 1)]]


skips :: [a] -> [[a]]
skips lst = [getL n lst | n <- [1..(length lst)]]


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rs)
           | a < b && b > c = b : localMaxima (b:c:rs)
           | otherwise      = localMaxima (b:c:rs)
localMaxima _ = []



count :: [Integer] -> [Int]
count ls = map (\n -> length $ filter (== n) ls) [0..9]

getline :: [Int] -> Int -> String
getline ls n = [if i >= n then '*' else ' ' | i <- ls]

histogram :: [Integer] -> String
histogram ls = unlines (map (getline c) [m,(m-1)..1]) ++ "==========\n0123456789\n"
           where c = count ls
                 m = maximum c
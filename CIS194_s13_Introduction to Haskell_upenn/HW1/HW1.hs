
--1.
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
            | x <= 0 = []
            | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))


toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)


--Ex2.
doubleEvery :: [Integer] -> [Integer]
doubleEvery [] = []
doubleEvery [x] = [x]
doubleEvery (x:y:xs) = x:(2*y):(doubleEvery xs)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse.doubleEvery.reverse


--EX3.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


--Ex4.
validate :: Integer -> Bool
validate x = ((sumDigits . doubleEveryOther. toDigits $ x) `mod` 10) == 0


--EX5.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c 
           | x <= 0 = []
           | otherwise = (hanoi (x-1) a c b) ++ ((a,b): (hanoi (x-1) c b a)) 


--EX6.

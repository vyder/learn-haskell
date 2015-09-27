module Homework1 where
  -- Exercise 1
  toDigits :: Integer -> [Integer]
  toDigits n
    | n > 0 && n < 10 = [n]
    | n >= 10         = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise       = []

  toDigitsRev :: Integer -> [Integer]
  toDigitsRev n = reverse (toDigits n)

  -- Exercise 2
  -- Note: This feels like the most elegant solution
  -- with the concepts I know so far
  doubleEveryOther :: [Integer] -> [Integer]
  doubleEveryOther list = reverse (doubleEveryOtherHelper (reverse list))

  -- doubles every second element starting from the left
  doubleEveryOtherHelper :: [Integer] -> [Integer]
  doubleEveryOtherHelper []       = []
  doubleEveryOtherHelper [x]      = [x]
  doubleEveryOtherHelper (x:y:ys) = [x, y*2] ++ doubleEveryOtherHelper ys

  -- Exercise 3
  sumDigits :: [Integer] -> Integer
  sumDigits []     = 0
  sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

  -- Exercise 4
  validate :: Integer -> Bool
  validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

  -- Exercise 5
  type Peg  = String
  type Move = (Peg, Peg)
  -- Integer is the number of Disks to move
  -- First  Peg is the Source
  -- Second Peg is the Destination
  -- Third  Peg is the Staging
  hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
  hanoi 1 a b _c = [(a,b)]
  hanoi n a  b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a

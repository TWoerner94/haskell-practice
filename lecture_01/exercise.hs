{-# OPTIONS_GHC -Wall #-}

lastDigit :: Integer -> Integer
lastDigit x = x - ((div x 10) * 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev xs
  | xs < 1             = []
  | lastDigit xs == xs = [xs]
  | otherwise          = lastDigit xs : toDigitsRev (div xs 10)

toDigits :: Integer -> [Integer]
toDigits xs = reverse (toDigitsRev xs)

doubleSecondElement :: [Integer] -> [Integer]
doubleSecondElement xs = head xs : [2 * (head (tail xs))]

doubleFirstElement :: [Integer] -> [Integer]
doubleFirstElement xs = 2 * (head xs) : [head( tail xs)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | length xs == 1   = xs 
  | length xs == 2   = doubleFirstElement xs
  | odd (length xs)  = doubleSecondElement xs ++ doubleEveryOther (tail (tail xs))
  | otherwise        = doubleFirstElement xs ++ doubleEveryOther (tail (tail xs))

sumDigits :: [Integer] -> Integer
sumDigits xs
  | length xs == 1 = sum (toDigits (head xs))
  | head xs > 9    = sum (toDigits (head xs)) + sumDigits (tail xs)
  | otherwise      = head xs + sumDigits (tail xs)

calcChecksum :: Integer -> Integer
calcChecksum x = mod (sumDigits (doubleEveryOther (toDigits x))) 10

validate :: Integer -> Bool
validate x
  | calcChecksum x == 0 = True
  | otherwise           = False

-- EXERCISE Towers Of Hanoi

type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a


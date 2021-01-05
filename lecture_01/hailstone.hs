{-# OPTIONS_GHC -Wall #-}

hailstone :: Integer -> Integer
hailstone n 
  | even n = div n 2
  | otherwise = (n*3) + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

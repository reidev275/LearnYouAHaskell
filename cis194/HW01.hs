lastDigit :: Integer -> Integer
lastDigit i = mod i 10

dropLastDigit :: Integer -> Integer
dropLastDigit i = div i 10

toRevDigits :: Integer -> [Integer]
toRevDigits i
  | i <= 0 = []
  | otherwise = (lastDigit i) : toRevDigits (dropLastDigit i)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x : y * 2 : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toRevDigits x) + sumDigits xs

luhn :: Integer -> Bool
luhn x =
  lastDigit (sumDigits (doubleEveryOther (toRevDigits x))) == 0


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,c)] ++ hanoi (n-1) b a c

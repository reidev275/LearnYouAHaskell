doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

tryhead :: [a] -> Maybe a
tryhead [] = Nothing
tryhead (x:_) = Just x


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element and it is: " ++ show x
tell (x:y:[]) = "The list has 2 elements and they are: " ++ show x ++ show y
tell (a:b:_) = "The list is long, its first two elements are: " ++ show a ++ show b

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."


calcBmis :: (RealFloat a) => [(a,a)] -> [a]
-- calcBmis xs = [bmi w h | (w,h) <- xs]
--   where bmi weight height = weight / height ^ 2

calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/h^2]

main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards"

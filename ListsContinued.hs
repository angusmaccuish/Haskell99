module ListsContinued where

import Lists(encode)

-- Problem 11
data Frequency a = Single a | Multiple Int a deriving (Eq, Show)
encodeModified :: Eq t => [t] -> [Frequency t]
encodeModified x = map toFrequency $ encode x
  where toFrequency f = case f of
          (1, value) -> Single value
          (n, value) -> Multiple n value

-- Problem 12
decodeModified :: [Frequency t] -> [t]
decodeModified [] = []
decodeModified ((Single a):xs) = a:decodeModified xs
decodeModified ((Multiple n a):xs) = (replicate n a) ++ decodeModified xs

-- Problem 13
encodeDirect :: [t] -> [Frequency t]
encodeDirect [] = []
encodeDirect x = error "TODO"

-- Problem 14
dupli :: [t] -> [t]
dupli xs = xs >>= \x -> x:x:[]

-- Problem 15
repli :: [t] -> Int -> [t]
repli xs n = xs >>= \x -> replicate n x

-- Problem 16
dropEvery :: [t] -> Int -> [t]
dropEvery [] _ = []
dropEvery xs 1 = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n 

-- Problem 17
split :: [t] -> Int -> ([t], [t])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [t] -> Int -> Int -> [t]
slice xs a b = drop (a-1) $ take b xs

-- Problem 19
rotate :: [t] -> Int -> [t]
rotate xs n 
  | n == 0    = xs
  | n > 0     = drop n xs ++ take n xs
  | otherwise = case split (reverse xs) (-n) of (a,b) -> reverse a ++ reverse b

-- Problem 20
removeAt :: Int -> [t] -> (t, [t])
removeAt k [] = error "Index out of range"
removeAt k xs = (y, x ++ ys) where (x,y:ys) = split xs (k-1)


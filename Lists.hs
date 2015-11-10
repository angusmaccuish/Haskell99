module Lists where

-- Problem 1
myLast :: [t] -> t
myLast []     = error "List cannot be empty"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [t] -> t
myButLast []       = error "List cannot be empty"
myButLast (x:[])   = error "List musy have at least one element"
myButLast (x:y:[]) = x
myButLast (x:xs)   = myButLast xs

-- Problem 3
elementAt :: [t] -> Int -> t
elementAt [] n = error "index out of range"
elementAt (x:xs) n = if n == 1 then x else elementAt xs (n-1)

-- Problem 4
myLength :: [t] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [t] -> [t]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq t => [t] -> Bool
isPalindrome [] = False
isPalindrome x = x == myReverse x

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList t -> [t]
flatten (Elem x) = [x]
flatten (List x) = foldr (++) [] $ map flatten x

-- Problem 8
compress :: Eq t => [t] -> [t]
compress [] = []
compress (x:[]) = [x]
compress (x:y:xs) = if x == y then compress (y:xs) else x:compress(y:xs)

-- Problem 9
pack :: Eq t => [t] -> [[t]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:xs) = case pack xs of
  y:ys -> if elem x y then (x:y):ys else [x]:y:ys

-- Problem 10
encode :: Eq t => [t] -> [(Int, t)]
encode = map (\x -> (length x, head x)) . pack

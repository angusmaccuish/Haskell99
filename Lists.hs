module Main where

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

main :: IO ()
main = do putStrLn $ show $ myLast [1,2,3,4] == 4
          putStrLn $ show $ myLast ['x','y','z'] == 'z'
          putStrLn $ show $ myButLast [1,2,3,4] == 3
          putStrLn $ show $ myButLast ['a'..'z'] == 'y'
          putStrLn $ show $ elementAt [1,2,3] 2 == 2
          putStrLn $ show $ elementAt "haskell" 5 == 'e'
          putStrLn $ show $ myLength [123, 456, 789] == 3
          putStrLn $ show $ myLength "Hello, world!" == 13
          putStrLn $ show $ myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A"
          putStrLn $ show $ myReverse [1,2,3,4] == [4,3,2,1]
          putStrLn $ show $ isPalindrome [1,2,3] == False
          putStrLn $ show $ isPalindrome "madamimadam" == True
          putStrLn $ show $ isPalindrome [1,2,4,8,16,8,4,2,1] == True 
          putStrLn $ show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
          putStrLn $ show $ flatten (List [] :: NestedList Int) == []
          putStrLn $ show $ compress "aaaabccaadeeee" == "abcade"
          putStrLn $ show $ pack ['a','a','a','a','b','c','c','a','a','d','e','e','e','e'] == ["aaaa","b","cc","aa","d","eeee"]
          putStrLn $ show $ encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
          putStrLn "Done"

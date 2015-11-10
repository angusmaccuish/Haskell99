module Main where

import Lists
import ListsContinued

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

{-
    Problem 9
    (**) Pack consecutive duplicates of list elements into sublists.
    
    If a list contains repeated elements they should be placed in separate sublists.

    Example:

    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))

    Example in Haskell:

    λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
                'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

list = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

pack :: Eq a => [a] -> [[a]]
pack [] = [] -- don't forget to keep it here
pack list@(a:_) = first : pack rest
    where (first, rest) = span (== a) list

pack' :: Eq a => [a] -> [[a]]
pack' = foldr concat' [[]]
    where concat' a [[]] = [[a]]
          concat' a (x:xs) | a == (head) x = ((a:x):xs) -- here (a:x) is the main point,
                                                    -- here we collect sublists ('e':"eee"):[[]]
                           | otherwise = ([a]:(x:xs)) -- here we initialize creation of       
                                                      -- a new sublist which we will grow in 
                                                      -- ['a']:["d","eeee"] equiv. "a" ["d","eeee"]

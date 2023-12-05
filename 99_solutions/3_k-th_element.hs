{- 
    (*) Find the K'th element of a list.

    The first element in the list is number 1. 
    
    Example:

    * (element-at '(a b c d e) 3)
    c

    Example in Haskell:

    λ> elementAt [1,2,3] 2
    2
    λ> elementAt "haskell" 5
    'e'
-}

list :: [Int]
list = [1,2,3,4,5]

{-
[1,2,3,4,5] 2
[2,3,4,5] 1
[3,4,5] 0
-}

elemAtEasy :: [a] -> Int -> a
elemAtEasy list  i = list !! pred i

elemAt :: [a] -> Int -> a
elemAt as i | null as = error "Empty List!"
            | length as < i
            || i <= 0 = error "Index Out Of Bound!"
elemAt (a:_) 1 = a 
elemAt (a:as) i = elemAt as $ pred i

elemAt' :: [a] -> Int -> a
elemAt' list i = head $ drop (pred i) list

-- elemAt'' :: [a] -> Int -> a


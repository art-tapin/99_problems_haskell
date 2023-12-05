{-

    Problem 8
    (**) Eliminate consecutive duplicates of list elements.   

    If a list contains repeated elements they should be replaced with 
    a single copy of the element. 
    The order of the elements should not be changed.

    Example:

    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)

    Example in Haskell:

    λ> compress "aaaabccaadeeee"
    "abcade"

-}

list = "aaabccaadeeee"
compress :: Eq a => [a] -> [a]
compress [a, b]   = if a == b then [a] else [a, b]
compress (a:b:xs) | a == b = compress (b:xs)
                  | otherwise = a : compress (b:xs)

compress' :: Eq a => [a] -> [a]
compress' = foldr comp []
    where comp a [] = [a] -- Notation acc@(x:xs) -> 'null acc' is incorrect, because it implies presence of x and xs
          comp a acc@(x:xs)
              | a == x = acc    -- If the current element is equal to the head of the accumulator, skip it
              | otherwise = a : acc  -- Otherwise, add it to the accumulator

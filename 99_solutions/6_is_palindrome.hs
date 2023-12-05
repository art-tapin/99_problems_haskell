{-

    Problem 6
    (*) Find out whether a list is a palindrome. Solutions

    Hint: A palindrome can be read forward or backward; 
          e.g. (x a m a x).

    Example in Haskell:

    λ> isPalindrome [1,2,3]
    False
    λ> isPalindrome "madamimadam"
    True
    λ> isPalindrome [1,2,4,8,16,8,4,2,1]
    True

-}

testString = "madamimadam"
notPal = "abcdefba"
intsNotPal = [1,2,3]
testInts = [1,2,4,8,16,8,4,2,1]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome as = as == reverse as

isPal :: Eq a => [a] -> Bool
isPal as = and (zipWith (==) as (reverse as))

-- from anwsers:
isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = (head xs == last xs) && isPalindrome' (init $ tail xs)
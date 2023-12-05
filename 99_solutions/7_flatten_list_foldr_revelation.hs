import System.CPUTime
import Text.Printf


{-
    Problem 7

    (**) Flatten a nested list structure.
    
    Transform a list, 
    possibly holding lists as elements into a `flat' list 
    by replacing each list with its elements (recursively).

    Example:

    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Example in Haskell:

    We have to define a new data type, 
    because lists in Haskell are homogeneous.

    data NestedList a = Elem a | List [NestedList a]

    λ> flatten (Elem 5)
    [5]
    λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    [1,2,3,4,5]
    λ> flatten (List [])
    []

-}

-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- foldr' _ acc [] = acc
-- foldr' f acc (a:as) = f a (foldr f acc as)

generateNestedList :: Int -> Int -> Int -> Int -> Int -> NestedList Int
generateNestedList level maxLevel start step numElements
  | level >= maxLevel = Elem start
  | otherwise = List $ take 4 [generateNestedList (level + 1) maxLevel (start + i * step) step numElements | i <- [0..numElements-1]]

-- Example Usage:
nestedListExample :: NestedList Int
nestedListExample = generateNestedList 1 10 1 2 5


data NestedList a = Elem a | List [NestedList a] deriving Show
list :: NestedList Integer
list = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

flatten :: NestedList a -> [a] -- Computation time: 2.501 sec
flatten nl = foldr unfold [] [nl]
    where unfold (Elem a) acc = a : acc
          unfold (List x) acc = foldr unfold acc x

-- from answers:
-- concatMap:
flatten' :: NestedList a -> [a] -- Computation time: 1.911 sec
flatten' (Elem e) = [e]
flatten' (List x) = concatMap flatten' x

-- recursive:
flattenRec :: NestedList a -> [a] -- Computation time: 2.045 sec
flattenRec (List [])     = []
flattenRec (Elem a)      = [a]
flattenRec (List (x:xs)) = flattenRec x ++ flattenRec (List xs)

{-
[L, []]
foldr a0 L [] => [1,2,3,4,5] => 7 unfold a6 => result = [1,2,3,4,5,[],[]] = [1,2,3,4,5]
        /
        \
       6. foldr a5 [E1, L, E5] []) => unfold a5 [] => a6 = [1,2,3,4,5,[]] 
                    \                                                      
                     \                                                     
                 5. foldr a4 E1 [E2, L, E5] => unfold E1 a4 => a5 = [1,2,3,4,5]
                                     \
                                      \
                                      foldr E2 [L,  E5] (it is right associative, so we start from rightmost)
                                            /   |     \
                                           /    |      \
                                          /     |       1. foldr a0 E5 [] => a1 = 5 : a0 = [5]      
                                         /      |            
                                        /       \ 
           4. unfold E2 a3 => a4 = [2,3,4,5]     \
                                                 foldr E3 E4
                                                       /    \
                                                      /      \
                                                     /        2. unfold E4 a1 => a2 = 4 : 5 = [4,5]
                                                    /
                                                   3. unfold E3 a2 => a3 = 3 : a2 = [3,4,5]
-}

{-
Thoughts:
    - foldr is right-associative => we go to the rightmost element and like in the signature:
        (a -> b -> b) == we take the rightmost element 'a' with accumulator 'b' and produce:
            b = a 'f' b, here we had (a : b) - creating a new list, and updating accumulator (b)

        Note: it recursively does that for every sub-level too,
        so that we had [5](l2), then [4,5](l4), then [3,4,5](l3) 
        because we branched at l2 having [L(l3,l4). Rightmost(E5)]

    - even though we jump into inner recursions, we still have a direction:
        foldr ... = f a _direction: (foldr ...)_, meaning we first go to the rightmost part of recursion
            i.e. even though our 'a', in 
                f 'a' (foldr rest) 
            can have a recursive call, we proceed firstly the rightmost recursion (foldr rest), 
            which in our example had a form of:

            6. foldr L1 (foldr []) => [1,2,3,4,5[]] = [1,2,3,4,5]
                       \           
                     5. foldr E1 (foldr [L2,E5]) => [1,2,3,4,5]
                                         /  \
                                        /     1. foldr E5 [] => [5]
                                       /
               4. foldr E2 (foldr L3) => [2,3,4,5]
                                  /
                                 /
           3. foldr E3 (foldr E4) => [3,4,5]
                               |
                                \
                                 2. foldr E4 [] => [4,5]
  
            where L1 = [E1, L2]
                  L2 = [E2, L3, E5]
                  L3 = [E3, E4]
            or
                  L1[E1,                      ]
                         L2[E2,            E5]
                                L3[E3, E4], 


-}

timeIt :: IO t -> IO ()
timeIt action = do
    startTime <- getCPUTime
    _ <- action
    endTime <- getCPUTime
    let diff = (fromIntegral (endTime - startTime)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)

-- Example of how to use `timeIt` with the flatten functions
main :: IO ()
main = do
    let testList = nestedListExample
    putStrLn "Testing foldr version"
    timeIt $ print $ flatten testList

    putStrLn "Testing concatMap version"
    timeIt $ print $ flatten' testList

    putStrLn "Testing recursive version"
    timeIt $ print $ flattenRec testList

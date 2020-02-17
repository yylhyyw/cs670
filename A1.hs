-- Question 1a

onlyABC :: [Char] -> Bool
onlyABC xs = null [ x | x <- xs, not $ elem x "aAbBcC"]

-- Question 1b

countPerfectSquares :: [Int] -> Int
countPerfectSquares [] = 0
countPerfectSquares xs = length [a | a <- [1..head xs], a*a == head xs] + countPerfectSquares (tail xs)

-- Question 1c

containsMatch :: Eq a => [(a, a)] -> Bool
containsMatch xs = not $ null [(a,b) | (a,b) <- xs, a == b]

-- Question 2a

isSymmetric :: Eq a => [(a, a)] -> Bool
isSymmetric [] = True
isSymmetric xs = and [elem (a, b) xs | (b, a) <- xs]
--test case: isSymmetric [(1, 3), (3, 1)] True
--test case: isSymmetric [('a', 'a'), ('b', 'm'), ('m', 'b')], True
--test case: isSymmetric [] True
--test case: isSymmetric [(5, 4), (1, 9)] False
--test case: isSymmetric [('b', 'a'), ('a', 'b'), ('z', 'x')] False
--test case: isSymmetric [(1, 2)] False


-- Question 2b

isTransitive :: Eq a => [(a, a)] -> Bool
isTransitive [] = True
isTransitive xs = and [elem (a, c) xs | (a, b) <- xs , (b', c) <- xs, b == b']
-- test case : isTransitive  [('b', 'a'), ('a', 'b'), ('b', 'b')] -- false
-- isTransitive  [(5, 4), (4, 9)] false
-- isTransitive [] True
-- isTransitive [(4, 1), (1, 4), (1, 1), (4, 4)] True
-- isTransitive [('a', 'b'), ('c', 'f')] True

-- Question 2c

isReflexive :: Eq a => [(a, a)] -> Bool
isReflexive [] = True
isReflexive xs = and [elem (a,a) xs | a <- (ys ++ zs)]
    where 
        ys = [x | (x,y) <- xs ]
        zs = [y | (x,y) <- xs ]
-- test case : isReflexive  [(1, 1), (3, 3)] True
-- isReflexive  [('a', 'a'), ('b', 'b'), ('b', 'x'), ('x', 'x')] false
-- isReflexive [] True
-- isReflexive  [(1, 2)] False
-- isReflexive  [('a', 'a'), ('c', 'd'), ('c', 'c')] False
-- isReflexive [(1, 2), (1, 1)] False

isEquivalenceRelation :: Eq a => [(a, a)] -> Bool
isEquivalenceRelation [] = True
isEquivalenceRelation xs = isSymmetric xs && isTransitive xs && isReflexive xs

-- Question 3

perfectNumbers :: Int -> [Int]
perfectNumbers n = take n [x | x <- [1..], (sum [i | i <- [1..x-1], x `mod` i == 0]) == x]

-- Question 4

isPrime :: Int -> Bool
isPrime n = n > 1 && null [x | x <- [2 .. div n 2], mod n x == 0]

primeFactors :: Int -> [Int]
primeFactors 0 = []
primeFactors 1 = []
primeFactors 2 = [2]
primeFactors n = [fpd] ++ (primeFactors (n `div` fpd))
    where 
        fpd = if isPrime n then n else head [x | x <- [2..n-1], n `mod` x == 0 && isPrime x]

-- Question 5

abaaMachine = [(1, 'a', 2), (2, 'a', 3), (2, 'b', 2), (3, 'a', 4)]

accept :: [(Integer, Char, Integer)] -> [Integer] -> [Char] -> Bool
accept machine accStates xs = runDFA 1 xs
    where runDFA 0     _  = False
          runDFA state [] = state `elem` accStates
          runDFA state (x:xs) = runDFA (nextState machine state x) xs
          nextState [] state x = 0
          nextState ((oldState, symbol, newState):ts) state x =
              if state == oldState && x == symbol then newState else nextState ts state x

accept' :: [(Integer, Char, Integer)] -> [Integer] -> [Char] -> Bool
accept' machine accStates xs = (foldl jump 1 xs) `elem` accStates
    where jump state x = if trans == [] then 0 else head trans 
              where trans = [nst | (ost, sym, nst) <- machine, ost == state, sym == x]

-- Question 6

multSpecial :: [Int] -> [Int] -> Integer
multSpecial [] (y:ys) = 0
multSpecial (x:xs) [] = 0
multSpecial (x:xs) (y:ys) = multSpecial' xs ys x y
    where multSpecial' :: [Int] -> [Int] -> Int -> Int -> Integer
          multSpecial' [] [] a b = toInteger $ a * b
          multSpecial' (x:xs) [] a b = multSpecial' xs [] ((a*10)+x) b
          multSpecial' [] (y:ys) a b = multSpecial' [] ys a ((b*10)+y)
          multSpecial' (x:xs) (y:ys) a b = multSpecial' xs ys ((a*10)+x) ((b*10)+y)

-- testcase : multSpecial [1,2] [1,2] 144
-- testcase : multSpecial [1,1,3] [1] 113
-- testcase : multSpecial [4,1,1] [] 0
-- testcase : multSpecial [0] [1,2,1] 0
-- testcase : multSpecial [2] [1,9,8] 396

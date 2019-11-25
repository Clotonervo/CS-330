
-- isPrime ***************************************
isPrime :: Int -> Bool
isPrime n = [x | x <- [2..(iSqrt n)], n `mod` x == 0] == []


-- iSqrt Helper function
iSqrt :: Int-> Int
iSqrt n = floor(sqrt(fromIntegral n))

-- Type Signature
primes :: [Int]
primes = [x | x <- [2.. ], isPrime x]


-- Type Signature
isPrimeFast :: Int -> Bool
isPrimeFast 2 = True
isPrimeFast n = [x | x <- takeWhile(<= iSqrt n) primesFast, n `mod` x == 0] == []

-- Type Signature
primesFast :: [Int]
primesFast = filter isPrimeFast [2.. ]

-- lcsLength ********************************************
lcsLength :: String -> String -> Int
lcsLength str1 str2 = a  where
  str1_length = length str1
  str2_length = length str2
                   a = array ((1,1),(n,n))
                        ([((1,j), 1) | j <- [1..n]] ++
                         [((i,1), 1) | i <- [2..n]] ++
                         [((i,j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                                     | i <- [2..n], j <- [2..n]])

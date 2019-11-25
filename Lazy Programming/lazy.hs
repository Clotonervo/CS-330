
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

-- Type Signature
lcsLength :: String -> String -> Int

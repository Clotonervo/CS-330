import Data.Array
import Prelude

-- isPrime ***************************************
isPrime :: Int -> Bool
isPrime n = [x | x <- [2..(iSqrt n)], n `mod` x == 0] == []


-- iSqrt Helper function
iSqrt :: Int-> Int
iSqrt n = floor(sqrt(fromIntegral n))

-- primes
primes :: [Int]
primes = [x | x <- [2.. ], isPrime x]


-- isPrimeFast 
isPrimeFast :: Int -> Bool
isPrimeFast 2 = True
isPrimeFast n = [x | x <- takeWhile(<= iSqrt n) primesFast, n `mod` x == 0] == []

-- primesFast
primesFast :: [Int]
primesFast = filter isPrimeFast [2.. ]

-- lcsLength ********************************************
lcsLength :: String -> String -> Int
lcsLength str1 str2 = a!(length str1,length str2)  where
    a = array ((0,0), (length str1,length str2))
      ([((i,0), 0) | i <- [0..length str1]] ++
      [((0,j), 0) | j <- [0..length str2]] ++
      [((i,j),
      if str1!!(i-1) == str2!!(j-1)
          then (a!(i-1,j-1)) + 1
          else max (a!(i-1, j)) (a!(i, j-1)))
          | i <- [1..length str1], j <- [1..length str2]])

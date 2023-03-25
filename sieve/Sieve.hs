primes :: [Int]
primes = 2 : [p | p <- [3,5..], isPrime p]
    where
        isPrime num = all (\p -> mod num p > 0) $ factorsToTry num
        factorsToTry num = takeWhile (\p -> p*p <= num) primes

sieve :: Int -> [Int]
sieve n = takeWhile (< n) primes

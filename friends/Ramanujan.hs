isPrime :: Int -> Bool
isPrime n | n < 2 = False
isPrime n = all (\p -> n `mod` p /= 0) . takeWhile ((<= n) . (^ 2)) $ primes

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

primeFactors :: Int -> [Int]
primeFactors n = iter n primes 
    where
        iter n [] = [n]
        iter n (p:_) | n < p ^ 2 = [n]
        iter n ps@(p:ps') = let (d, r) = n `divMod` p
                                in if r == 0 then p : iter d ps else iter n ps'


takeWhileSame :: Int -> [Int] -> [Int]
takeWhileSame _ [] = []
takeWhileSame x (y:ys) = if x == y then takeWhileSame x ys else y:ys


sumSameFactor :: [Int] -> Int -> Int
sumSameFactor [] _ = 0
sumSameFactor [x] exp = (x ^ exp) + 1
sumSameFactor (x:y:xs) exp =
    if x == y then
        (x ^ exp) + sumSameFactor (y:xs) (exp + 1)
    else
        (x ^ exp) + 1


ramanujan :: [Int] -> Int
ramanujan [] = 1
ramanujan ps@(x:_) = sumSameFactor ps 1 * ramanujan (takeWhileSame x ps) 


searchFriendOf :: Int -> (Int, Int)
searchFriendOf n = 
    let sm1 = (ramanujan . primeFactors) n - n
        in  if (ramanujan . primeFactors) sm1 - sm1 == n && sm1 /= n && sm1 > n then 
                (n, sm1) 
            else 
                (0,0)

friends :: Int -> [(Int, Int)]
friends limit = filter (\(a,b) -> a /= 0) $ map searchFriendOf [1..limit]

main :: IO ()
main = print $ friends 1000000
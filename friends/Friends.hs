sigma1 :: Int -> Int
sigma1 n = sum $ 1 : filter (\x -> mod n x == 0) [2..div n 2]

searchFriendOf :: Int -> (Int, Int)
searchFriendOf n = let sm1 = sigma1 n in if sigma1 sm1 == n && sm1 /= n && sm1 > n then (n, sm1) else (0,0)

friends :: Int -> [(Int, Int)]
friends limit = filter (\(a,b) -> a /= 0) $ map searchFriendOf [1..limit]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
    | null factors = [n]
    | otherwise = factors ++ primeFactors (n `div` head factors)
    where factors = take 1 $ filter (\x -> mod n x == 0) [2..n - 1]

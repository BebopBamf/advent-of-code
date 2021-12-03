module Three (threePartOne) where


threePartOne :: String -> [Int]
threePartOne = sumArray . parseToIntArray


parseToIntArray :: String -> [[Int]]
parseToIntArray = map parseToInt . lines


parseToInt :: String -> [Int]
parseToInt = map (\c -> read [c])


sumArray :: [[Int]] -> [Int]
sumArray [] = []
sumArray [x] = x
sumArray (x : xs) = zipWith (+) x $ sumArray xs

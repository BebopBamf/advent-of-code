module One (
    onePartOne,
    onePartTwo,
) where

import System.IO (IOMode (ReadMode), hGetContents, withFile)


toArray :: String -> [Int]
toArray contentStr =
    map read $ lines contentStr


toArrayWithWindow :: [Int] -> [Int]
toArrayWithWindow arr = zipWith (+) arr (zipWith (+) (tail arr) $ tail (tail arr))


countLarger :: String -> Int
countLarger content =
    sumRunningLarge $ toArray content


sumRunningLarge :: [Int] -> Int
sumRunningLarge [] = 0
sumRunningLarge [_] = 0
sumRunningLarge (x : xs)
    | x < head xs = 1 + sumRunningLarge xs
    | otherwise = sumRunningLarge xs


onePartOne :: String -> Int
onePartOne = sumRunningLarge . toArray


onePartTwo :: String -> Int
onePartTwo = sumRunningLarge . toArrayWithWindow . toArray
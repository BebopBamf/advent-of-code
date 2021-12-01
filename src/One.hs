module One (
    readData,
) where

import System.IO (IOMode (ReadMode), hGetContents, withFile)


readData :: IO ()
readData =
    withFile
        "resources/day_1/input.txt"
        ReadMode
        ( \handle -> do
            contents <- hGetContents handle
            print $ sumRunningLarge $ toArrayWithWindow $ toArray contents
        )


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
sumRunningLarge [x] = 0
sumRunningLarge (x : xs)
    | x < head xs = 1 + sumRunningLarge xs
    | otherwise = sumRunningLarge xs
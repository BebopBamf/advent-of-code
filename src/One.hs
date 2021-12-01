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
            print $ countLarger contents
        )


countLarger :: String -> Int
countLarger content =
    sumRunningLarge . map read $ lines content


sumRunningLarge :: [Int] -> Int
sumRunningLarge [] = 0
sumRunningLarge [x] = 0
sumRunningLarge (x : xs)
    | x < head xs = 1 + sumRunningLarge xs
    | otherwise = sumRunningLarge xs
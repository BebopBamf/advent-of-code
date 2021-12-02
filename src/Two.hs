{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Two (twoPartOne) where


data DiveReference
    = Forward Int
    | Up Int
    | Down Int
    | Other


twoPartOne :: String -> Int
twoPartOne input =
    let diveRef = parseToReferenceList input
        depth = getDepth diveRef
        horPos = getHorizontalPos diveRef
     in depth * horPos


parseToReferenceList :: String -> [DiveReference]
parseToReferenceList = map ((\(x : xs) -> parseDiveRefVal x $ head xs) . words) . lines


parseDiveRefVal :: String -> String -> DiveReference
parseDiveRefVal "forward" s = Forward $ read s
parseDiveRefVal "up" s = Up $ read s
parseDiveRefVal "down" s = Down $ read s
parseDiveRefVal _ _ = Other


getDepthVal :: DiveReference -> Int
getDepthVal (Up x) = - x
getDepthVal (Down x) = x
getDepthVal _ = 0


getDepth :: [DiveReference] -> Int
getDepth = sum . map getDepthVal


getHorizontalPosVal :: DiveReference -> Int
getHorizontalPosVal (Forward x) = x
getHorizontalPosVal _ = 0


getHorizontalPos :: [DiveReference] -> Int
getHorizontalPos = sum . map getHorizontalPosVal
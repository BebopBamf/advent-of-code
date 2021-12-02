module Two (twoPartOne, twoPartTwo) where


data DiveReference
    = Forward Int
    | Up Int
    | Down Int
    | Other


data DiveValues = DiveValues
    { aim :: Int
    , depth :: Int
    , horizontalPosition :: Int
    }
    deriving (Show)


initialDiveValue :: DiveValues
initialDiveValue = DiveValues 0 0 0


twoPartOne :: String -> Int
twoPartOne = diveValueToInt . foldl toDiveValueOne initialDiveValue . parseToReferenceList


twoPartTwo :: String -> Int
twoPartTwo = diveValueToInt . foldl toDiveValueTwo initialDiveValue . parseToReferenceList


parseToReferenceList :: String -> [DiveReference]
parseToReferenceList = map ((\(x : xs) -> parseDiveRefVal x $ head xs) . words) . lines


parseDiveRefVal :: String -> String -> DiveReference
parseDiveRefVal "forward" s = Forward $ read s
parseDiveRefVal "up" s = Up $ read s
parseDiveRefVal "down" s = Down $ read s
parseDiveRefVal _ _ = Other


toDiveValueOne :: DiveValues -> DiveReference -> DiveValues
toDiveValueOne state (Forward x) = state {horizontalPosition = horizontalPosition state + x}
toDiveValueOne state (Up x) = state {depth = depth state - x}
toDiveValueOne state (Down x) = state {depth = depth state + x}
toDiveValueOne state Other = state


toDiveValueTwo :: DiveValues -> DiveReference -> DiveValues
toDiveValueTwo state (Forward x) = state {horizontalPosition = horizontalPosition state + x, depth = depth state + (aim state * x)}
toDiveValueTwo state (Up x) = state {aim = aim state - x}
toDiveValueTwo state (Down x) = state {aim = aim state + x}
toDiveValueTwo state Other = state


diveValueToInt :: DiveValues -> Int
diveValueToInt DiveValues {horizontalPosition = h, depth = d} = h * d

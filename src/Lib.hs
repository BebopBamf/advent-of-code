module Lib (
    ChallengeReference (..),
    runCode,
) where

import One (onePartOne, onePartTwo)
import Two (twoPartOne)

import System.IO (IOMode (ReadMode), hGetContents, withFile)


data ChallengeReference
    = DayOnePartOne
    | DayOnePartTwo
    | DayTwoPartOne


--    | DayTwoPartTwo

runCode :: ChallengeReference -> IO ()
runCode DayOnePartOne = printResult onePartOne "resources/day_1/input.txt"
runCode DayOnePartTwo = printResult onePartTwo "resources/day_1/input.txt"
runCode DayTwoPartOne = printResult twoPartOne "resources/day_2/input.txt"


-- runCode DayOnePartTwo = printResult onePartTwo "resources/day_1/input.txt"

printResult :: Show a => (String -> a) -> String -> IO ()
printResult transform path =
    withFile
        path
        ReadMode
        ( \handle -> do
            contents <- hGetContents handle
            print $ transform contents
        )

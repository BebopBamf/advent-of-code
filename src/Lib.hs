module Lib (
    ChallengeReference (..),
    runCode,
) where

import One (onePartOne, onePartTwo)
import Three (threePartOne)
import Two (twoPartOne, twoPartTwo)

import System.IO (IOMode (ReadMode), hGetContents, withFile)


data ChallengeReference
    = DayOnePartOne
    | DayOnePartTwo
    | DayTwoPartOne
    | DayTwoPartTwo
    | DayThreePartOne


runCode :: ChallengeReference -> IO ()
runCode DayOnePartOne = printResult onePartOne "resources/day_1/input.txt"
runCode DayOnePartTwo = printResult onePartTwo "resources/day_1/input.txt"
runCode DayTwoPartOne = printResult twoPartOne "resources/day_2/input.txt"
runCode DayTwoPartTwo = printResult twoPartTwo "resources/day_2/input.txt"
runCode DayThreePartOne = printResult threePartOne "resources/day_3/test.txt"


printResult :: Show a => (String -> a) -> String -> IO ()
printResult transform path =
    withFile
        path
        ReadMode
        ( \handle -> do
            contents <- hGetContents handle
            print $ transform contents
        )

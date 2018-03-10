-- Haskell Laboratory 2. --
import Prelude
import Data.Char

-- A)
-- write function:
readInts :: String -> [Int]
-- which will natural numbers from given text, example:
-- *Main> readInts "1 23 456 7.8 abc 9"
-- [1,23,456,9]
-- *Main> readInts "foo"
-- []
-- you should use isDigit function from Data.Char module
-- you should also use functions map, filter, all from Prelude
readInts "" = []
readInts text = map (read) (filter (all isDigit) (words text))

-- B)
-- write similar function readInts2
readInts2 :: String -> Either String [Int]
-- which produces list of numbers or otherwise an error message (if any word is not a number)
-- Example:
-- *Main> readInts2  "1 23 456 foo 9"
-- Left "Not a number: foo"
-- *Main> readInts2  "1 23 456"     
-- Right [1,23,456]
readInts2 text = if length (filter (all (isDigit)) numbers) < length numbers then
                    Left "There is a non-number word."
                 else
                    Right (readInts text)
                 where numbers = words text

-- C)
-- write a function:
sumInts :: String -> String
-- if every word of an argument are a numbers, it should produce their sum
-- otherwise return an error
sumInts text = helper (readInts2 text)
        where helper (Left e) = e
              helper (Right ns) = show (sum ns)
-- create a program running sumInts using interact
main = do
    text <- getLine
    putStrLn (sumInts text)

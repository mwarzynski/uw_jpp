-- Haskell Laboratory 3. --
import Prelude
import Data.Char
import Text.Read

readInts2 = Text.Read.readEither

sumInts str = case readInt2 str of
                Left err   -> err
                Right ints -> show $ foldr (+1) 0 ints



-- Haskell Laboratory 3. --
import Control.Monad.Error
import Data.Char

data ParseError = Err {location::Int, reason::String} deriving Show

instance Error ParseError where
    noMsg    = Err (-1) "Unknown error."
    strMsg s = Err (-1) s

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c location
  | c >= '0' && c <= '9' = 
      Right $ fromIntegral $ (ord c) - (ord '0')
  | c >= 'a' && c <= 'f' =
      Right $ fromIntegral $ (ord c) - (ord 'a') + 10
  |otherwise = Left (Err location $ " '" ++ [c] ++ "' character is not a hex digit")

parseHex :: String -> ParseMonad Integer
parseHex [] = Left $ Err 0 "Empty string."
parseHex text = foldl f (Right 0) $ fmap (helper) (zip text [1..])
    where helper (c, loc) = case (parseHexDigit c loc) of 
                              Left e  -> Left e
                              Right n -> Right (n*16^(lt - loc))
          lt = length text
          f :: (ParseMonad Integer) -> (ParseMonad Integer) -> ParseMonad Integer
          f n a = case n of
                    Left e -> Left e
                    Right k -> case a of
                                 Left e -> Left e
                                 Right l -> Right (k + l)

toString :: Integer -> ParseMonad String
toString n = return $ show n

-- convert converts a string with hex numbers to string with int numbers
convert :: String -> String
convert s = str where
             (Right str) = tryParse s `catchError` printError
             tryParse s = do {n <- parseHex s; toString n}
             printError e = Left e
 

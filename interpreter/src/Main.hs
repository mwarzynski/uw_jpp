import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Except

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar

import ErrM

import Interpreter


type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
           Bad _ -> do putStrLn "Parsing code failed."
                       exitFailure
           Ok  _ -> interpret ts

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "The Interpreter."
    , ""
    , "usage: Call with one of the following argument combinations:"
    , "  --help, -h      Display this help message."
    , "  (no arguments)  Interpret stdin."
    , "  -f (file)       Interpret content of given file."
    ]
  exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> usage
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run pProgram
    "-f":fs -> mapM_ (runFile pProgram) fs


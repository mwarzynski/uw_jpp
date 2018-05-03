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

import Interpreter (interpret)


runFile :: FilePath -> IO ()
runFile f = readFile f >>= run

run :: String -> IO ()
run text = let tokens = myLexer text in case pProgram tokens of
           Bad _       -> do putStrLn "Parsing code failed."
                             exitFailure
           Ok  program -> interpret program

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
    [] -> hGetContents stdin >>= run
    "-f":fs -> mapM_ (runFile) fs


module Interpreter where

import System.IO ( stdin, hGetContents )
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

interpret :: Program -> IO ()
interpret program = putStrLn (show program)



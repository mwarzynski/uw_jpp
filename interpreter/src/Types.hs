module Types where

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Except

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar

import ErrM


typesAnalyze :: Program -> Err Bool
typesAnalyze ps = return True


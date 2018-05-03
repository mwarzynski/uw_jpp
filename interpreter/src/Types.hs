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


analyzeTypes :: Program -> Err Bool
analyzeTypes ps = return True


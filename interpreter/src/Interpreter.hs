module Interpreter where

import Data.Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import ErrM

import AbsGrammar
import PrintGrammar


data IVal = Int | Float | Bool | String deriving (Show, Eq, Ord)

type IVar   = Ident
type IFName = Ident
type ILoc   = Int

newtype IFun = IFun ([IVal] -> Interpreter IVal)

type IStore = Map ILoc IVal

type IEnvVar  = Map IVar   ILoc
type IEnvFunc = Map IFName IFun
type IEnv     = (IEnvVar, IEnvFunc)

type IResult = ExceptT String IO
type Interpreter a = StateT IStore (ReaderT IEnv IResult) a


setVar :: IVar -> IVal -> Interpreter IEnv
setVar var value = do
    (envVar, envFunc) <- ask
    return (insert var value envVar, envFunc)

setFunc :: IFName -> IFun -> Interpreter IEnv 
setFunc fname fun = do
    (envVar, envFun) <- ask
    return (envVar, insert fname fun envFun)

parseDeclarations :: [Decl] -> Interpreter IEnv
parseDeclarations declarations = do
    return (empty, empty)

interpretProgram :: Program -> Interpreter ()
interpretProgram (Prog declarations) = do
    putStrLn (show declarations)
    env <- parseDeclarations declarations
    return ()

interpret :: Program -> IResult ()
interpret program = do
    runReaderT (execStateT (interpretProgram program) empty) (empty, empty)
    return ()    


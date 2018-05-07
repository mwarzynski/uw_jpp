module Interpreter where

import Data.Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import ErrM

import AbsGrammar
import PrintGrammar


data IVal = IInt Int | IFloat Float | IBool Bool | IString String deriving (Show, Eq, Ord)

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


-- setVar :: IVar -> IVal -> Interpreter IEnv
-- setVar var value = do
--     (envVar, envFunc) <- ask
--     return (insert var value envVar, envFunc)

setFunc :: IFName -> IFun -> Interpreter IEnv 
setFunc fname fun = do
    (envVar, envFun) <- ask
    return (envVar, insert fname fun envFun)

-- Parse

parseVarS :: VarS -> Interpreter IEnv
parseVarS vars = case vars of
    Dec name vtype -> do
        env <- ask
        liftIO $ putStrLn (show name)
        liftIO $ putStrLn (show vtype)
        return env
    DecMany names vtype -> do
        env <- ask
        liftIO $ putStrLn (show names)
        liftIO $ putStrLn (show vtype)
        return env
    DecSet name vtype exp -> do
        env <- ask
        liftIO $ putStrLn (show name)
        liftIO $ putStrLn (show vtype)
        liftIO $ putStrLn (show exp)
        return env

parseVarE :: VarE -> Interpreter IEnv
parseVarE vare = do
    env <- ask
    case vare of
        DecStruct name struct -> return env
        DecDict name keyType valueType -> return env
        DecArr name iType values -> return env
        DecArrMul name iType length -> return env
        DecArrMulInit name iType length item -> return env

parseVar :: Var -> Interpreter IEnv
parseVar var = case var of
    DVarS vars -> parseVarS vars
    DVarE vare -> parseVarE vare

parseVars :: [Var] -> Interpreter IEnv
parseVars [] = do
    env <- ask
    return env
parseVars (v:vs) = do
    env <- parseVar v
    env2 <- local (const env) $ parseVars vs
    return env2

parseDFunction :: Function -> Interpreter IEnv
parseDFunction f = case f of
    FunStr func args rstr stms -> do
        env <- ask
        return env
    FunOne func args rtype stms -> do
        env <- ask
        return env
    FunNone func args stms -> do
        executeStatements stms
        env <- ask
        --let f a = do
        --    env1 <- local (const env) $ (setArguments args a)
        --    return $ Int 0
        return env

parseDStruct :: Struct -> Interpreter IEnv
parseDStruct struct = do
    env <- ask
    return env

parseDVar :: Var -> Interpreter IEnv
parseDVar var = do
    env <- ask
    return env

parseDeclaration :: Decl -> Interpreter IEnv
parseDeclaration declaration = case declaration of
    DFunction func -> parseDFunction func
    DStruct struct -> parseDStruct struct
    DVar var       -> parseDVar var

parseDeclarations :: [Decl] -> Interpreter IEnv
parseDeclarations [] = do
    env <- ask
    return env
parseDeclarations (d:ds) = do
    envl <- parseDeclaration d
    env <- local (const envl) $ parseDeclarations ds
    return env

-- Exec

executePrint :: [IVal] -> Interpreter ()
executePrint [] = liftIO $ putStr "\n"
executePrint (v:vs) = do
    case v of
        IInt i    -> liftIO $ putStr (show i)
        IFloat f  -> liftIO $ putStr (show f)
        IBool b   -> liftIO $ putStr (show b)
        IString s -> liftIO $ putStr (s)
    executePrint vs

executeExp :: Exp -> Interpreter IVal
executeExp e = case e of
    EStr str -> return $ IString str
    Call func exps -> do
        vals <- mapM executeExp exps
        executePrint vals
        return $ IInt 0

executeStatement :: Stm -> Interpreter IEnv
executeStatement s = do
    env <- ask
    case s of
        SFunc f -> do
            liftIO $ putStrLn (show f)
            return env
        SDecl d -> do
            liftIO $ putStrLn (show d)
            return env
        SExp  e -> do
            val <- executeExp e
            return env


executeStatements :: [Stm] -> Interpreter ()
executeStatements [] = return ()
executeStatements (s:ss) = do
    env <- executeStatement s
    env2 <- local (const env) $ executeStatements ss
    return ()


-- Interpret

interpretProgram :: Program -> Interpreter ()
interpretProgram (Prog declarations) = do
    env <- parseDeclarations declarations
    return ()

interpret :: Program -> IResult ()
interpret program = do
    runReaderT (execStateT (interpretProgram program) empty) (empty, empty)
    return ()    


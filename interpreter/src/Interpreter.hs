module Interpreter where

import Data.Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import ErrM

import AbsGrammar
import PrintGrammar


data IVal
    = IInt Int
    | IFloat Float
    | IBool Bool
    | IString String
    deriving (Show, Eq, Ord)

type IVar   = Ident
type IFName = Ident
type ILoc   = Integer

newtype IFun = IFun ([IVal] -> Interpreter IVal)

type IStore = Map ILoc IVal

type IEnvVar  = Map IVar   ILoc
type IEnvFunc = Map IFName IFun
type IEnv     = (IEnvVar, IEnvFunc)

type IResult = ExceptT String IO
type Interpreter = StateT IStore (ReaderT IEnv IResult)

-- Initial environment

funcPrint :: [IVal] -> Interpreter IVal
funcPrint [] = do
    liftIO $ putStr "\n"
    return $ (IInt 0)
funcPrint (v:vs) = do
    case v of
        IInt i    -> liftIO $ putStr (show i)
        IFloat f  -> liftIO $ putStr (show f)
        IBool b   -> liftIO $ putStr (show b)
        IString s -> liftIO $ putStr (s)
    funcPrint vs

initializeEnv :: Interpreter IEnv
initializeEnv = do
    env <- ask
    env1 <- local (const env) $ setFun (Ident "print") (IFun funcPrint)
    return env1

-- Store & Environment

newLoc :: Interpreter ILoc
newLoc = do
    store <- get
    let loc = if size store > 0 then (fst $ findMax store) + 1 else 1
    return $ loc

getVarLoc :: IVar -> Interpreter ILoc
getVarLoc var = do
    (env, _) <- ask
    if member var env then
        return $ env ! var
    else
        throwError ("Var " ++ (show var) ++ " does not exit.")

setVarLoc :: IVar -> ILoc -> Interpreter IEnv
setVarLoc var loc = do
    (envVar, envFun) <- ask
    return (insert var loc envVar, envFun)

getLocVal :: ILoc -> Interpreter IVal
getLocVal loc = do
    store <- get
    if member loc store then
        return (store ! loc)
    else
        throwError ("There is no value for loc=" ++ (show loc))

setLocVal :: ILoc -> IVal -> Interpreter ()
setLocVal loc val = do
    store <- get
    modify (\s -> insert loc val s)

setFun :: IFName -> IFun -> Interpreter IEnv
setFun fname fun = do
    (envVar, envFun) <- ask
    return (envVar, insert fname fun envFun)

getFun :: IFName -> Interpreter IFun
getFun fname = do
    (_, env) <- ask
    if member fname env then
        return (env ! fname)
    else
        throwError ("Func " ++ (show fname) ++ " does not exist.")

-- Parse

parseVarS :: VarS -> Interpreter ([(IVar, IVal)])
parseVarS vars = case vars of
    Dec name vtype -> do
        liftIO $ putStrLn (show name)
        liftIO $ putStrLn (show vtype)
        return $ [(name, IInt(0))]

-- parseVarE :: VarE -> Interpreter ([(IVar, IVal)])
-- parseVarE vare = do
--     env <- ask
--     case vare of
--         DecStruct name struct -> return env
--         DecDict name keyType valueType -> return env
--         DecArr name iType values -> return env
--         DecArrMul name iType length -> return env
--         DecArrMulInit name iType length item -> return env

parseVar :: Var -> Interpreter ([(IVar, IVal)])
parseVar var = case var of
    DVarS vars -> parseVarS vars
    --DVarE vare -> parseVarE vare

parseBindArgument :: [(IVar, IVal)] -> Interpreter IEnv
parseBindArgument (t:ts) = do
    env <- ask
    loc <- newLoc
    let var = (fst t)
    let val = (snd t)
    env1 <- local (const env) $ setVarLoc var loc
    setLocVal loc val
    env2 <- local (const env1) $ parseBindArgument ts
    return env2

parseBindArguments :: [Var] -> [IVal] -> Interpreter IEnv
parseBindArguments [] [] = ask
parseBindArguments (var:vars) (val:vals) = do
    env <- ask
    pvars <- parseVar var
    env1 <- local (const env) $ parseBindArgument pvars
    env2 <- local (const env1) $ parseBindArguments vars vals
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
        env <- ask
        let fname vals = do
            -- Bind arguments to passed values.
            env1 <- local (const env) $ parseBindArguments args vals
            -- Add function definition to the environment as to allow
            --  recursive function calling.
            env2 <- local (const env1) $ setFun func (IFun fname)
            -- Execute function statements in the new environment.
            local (const env2) $ executeStatements stms
            -- Return nothing.
            return $ IInt 0
        envS <- local (const env) $ setFun func (IFun fname)
        return envS

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
parseDeclarations [] = ask
parseDeclarations (d:ds) = do
    envl <- parseDeclaration d
    env <- local (const envl) $ parseDeclarations ds
    return env

-- Exec

executeExp :: Exp -> Interpreter IVal
executeExp e = case e of
    EStr str -> return $ IString str
    Call func exps -> do
        (IFun f) <- getFun func
        vals <- mapM executeExp exps
        f vals

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
    env <- initializeEnv
    env1 <- local (const env) $ parseDeclarations declarations
    (IFun main) <- local (const env1) $ getFun (Ident "main")
    local (const env1) $ main []
    return ()

interpret :: Program -> IResult ()
interpret program = do
    store <- runReaderT (execStateT (interpretProgram program) empty) (empty, empty)
    -- liftIO $ putStrLn (show store)
    return ()    


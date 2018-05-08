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
    = IInt Integer
    | IFloat Double
    | IBool Bool
    | IString String
    | Null
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

data IJump = INothing | IBreak | IContinue | IReturn IVal deriving (Show)

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
        throwError ("Variable " ++ (show var) ++ " does not exit.")

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

defaultTypeValue :: Type -> IVal
defaultTypeValue TInt = IInt 0
defaultTypeValue TFloat = IFloat 0.0
defaultTypeValue TStr = IString ""
defaultTypeValue TBool = IBool False

parseTokenBool :: TokenBool -> Bool
parseTokenBool token = if token == (TokenBool "true") then True else False

parseVarS :: VarS -> Interpreter ([(IVar, IVal)])
parseVarS vars = case vars of
        Dec name vtype -> do
            return $ [(name, (defaultTypeValue vtype))]
        DecSet name vtype exp -> do
            val <- executeExp exp
            return $ [(name, val)]
        _ -> do
            throwError ("Not implemented: " ++ (show vars))
            return $ []

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

bindValues :: [(IVar, IVal)] -> Interpreter IEnv
bindValues [] = ask
bindValues (t:ts) = do
    env <- ask
    loc <- newLoc
    let var = (fst t)
    let val = (snd t)
    env1 <- local (const env) $ setVarLoc var loc
    setLocVal loc val
    env2 <- local (const env1) $ bindValues ts
    return env2

parseBindArguments :: [Var] -> [IVal] -> Interpreter IEnv
parseBindArguments [] [] = ask
parseBindArguments (var:vars) (val:vals) = do
    env <- ask
    pvars <- parseVar var
    -- TODO: acknowledge passed value (val)
    env1 <- local (const env) $ bindValues pvars
    env2 <- local (const env1) $ parseBindArguments vars vals
    return env2

parseDFunction :: Function -> Interpreter IEnv
parseDFunction f = case f of
    FunOne func args rtype stms -> do
        env <- ask
        let fname vals = do
            -- Bind arguments to passed values.
            env1 <- local (const env) $ parseBindArguments args vals
            -- Add function definition to the environment as to allow
            --  recursive function calling.
            env2 <- local (const env1) $ setFun func (IFun fname)
            -- Execute function statements in the new environment.
            (env3, val) <- local (const env2) $ executeStatements stms
            -- Return one value of standard type.
            case val of
                IReturn v -> return $ v
                _ -> throwError "Function without return value"
        envS <- local (const env) $ setFun func (IFun fname)
        return envS
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

parseDeclaration :: Decl -> Interpreter IEnv
parseDeclaration declaration = case declaration of
    DFunction func -> parseDFunction func
    _ -> throwError ("parseDeclaration: Not implemented: " ++ (show declaration))

parseDeclarations :: [Decl] -> Interpreter IEnv
parseDeclarations [] = ask
parseDeclarations (d:ds) = do
    env <- parseDeclaration d
    env1 <- local (const env) $ parseDeclarations ds
    return env1

-- Exec

iValAdd :: IVal -> IVal -> Interpreter IVal
iValAdd (IInt a) (IInt b) = return $ IInt (a + b)
iValAdd (IFloat a) (IFloat b) = return $ IFloat (a + b)
iValAdd (IString a) (IString b) = return $ IString (a ++ b)
iValAdd v1 v2 = throwError ("Adding invalid types: " ++ (show v1) ++ " + " ++ (show v2))

iValSub :: IVal -> IVal -> Interpreter IVal
iValSub (IInt a) (IInt b) = return $ IInt (a - b)
iValSub (IFloat a) (IFloat b) = return $ IFloat (a - b)
iValSub v1 v2 = throwError ("Subtracting invalid types: " ++ (show v1) ++ " - " ++ (show v2))

iValMul :: IVal -> IVal -> Interpreter IVal
iValMul (IInt a) (IInt b) = return $ IInt (a * b)
iValMul (IFloat a) (IFloat b) = return $ IFloat (a * b)
iValMul v1 v2 = throwError ("Mulitplication of invalid types: " ++ (show v1) ++ " * " ++ (show v2))

iValDiv :: IVal -> IVal -> Interpreter IVal
iValDiv (IFloat a) (IFloat b) = if b /= 0.0 then
                                    return $ IFloat (a / b)
                                else throwError "Division by zero"
iValDiv v1 v2 = throwError ("Division of invalid types: " ++ (show v1) ++ " / " ++ (show v2))

iValEq :: IVal -> IVal -> Interpreter IVal
iValEq v1 v2 = case v1 of
    IInt i1 -> case v2 of
        IInt i2 -> if i1 == i2 then return $ IBool True else return $ IBool False
        _ -> throwError ("Comparing different types: " ++ (show v1) ++ " + " ++ (show v2))
    IFloat f1 -> case v2 of
        IFloat f2 -> if f1 == f2 then return $ IBool True else return $ IBool False
        _ -> throwError ("Comparing different types: " ++ (show v1) ++ " + " ++ (show v2))
    IString s1 -> case v2 of
        IString s2 -> if s1 == s2 then return $ IBool True else return $ IBool False
        _ -> throwError ("Comparing different types: " ++ (show v1) ++ " + " ++ (show v2))
    IBool b1 -> case v2 of
        IBool b2 -> if b1 == b2 then return $ IBool True else return $ IBool False
        _ -> throwError ("Comparing different types: " ++ (show v1) ++ " + " ++ (show v2))

iValLt :: IVal -> IVal -> Interpreter IVal
iValLt v1 v2 = case v1 of
    IInt i1 -> case v2 of
        IInt i2 -> if i1 < i2 then return $ IBool True else return $ IBool False
        _ -> throwError ("Comparing different types: " ++ (show v1) ++ " + " ++ (show v2))
    IFloat f1 -> case v2 of
        IFloat f2 -> if f1 < f2 then return $ IBool True else return $ IBool False
        _ -> throwError ("Comparing different types: " ++ (show v1) ++ " + " ++ (show v2))
    _ -> throwError ("Comparing not comparable types: " ++ (show v1) ++ " + " ++ (show v2))

executeEAss :: IVar -> Exp -> Interpreter IVal
executeEAss var exp = do
    loc <- getVarLoc var
    val <- executeExp exp
    setLocVal loc val
    return (IInt 1)

executeEAdd :: Exp -> Exp -> Interpreter IVal
executeEAdd e1 e2 = do
    v1 <- executeExp e1
    v2 <- executeExp e2
    v <- iValAdd v1 v2
    return v

executeEEq :: Exp -> Exp -> Interpreter IVal
executeEEq e1 e2 = do
    v1 <- executeExp e1
    v2 <- executeExp e2
    v <- iValEq v1 v2
    return v

executeELt :: Exp -> Exp -> Interpreter IVal
executeELt e1 e2 = do
    v1 <- executeExp e1
    v2 <- executeExp e2
    v <- iValLt v1 v2
    return v

executeEEPlus :: IVar -> Exp -> Interpreter IVal
executeEEPlus var exp = do
    val <- executeExp exp
    loc <- getVarLoc var
    cval <- getLocVal loc
    nval <- iValAdd cval val
    setLocVal loc nval
    return (nval)

executeEEMinus :: IVar -> Exp -> Interpreter IVal
executeEEMinus var exp = do
    val <- executeExp exp
    loc <- getVarLoc var
    cval <- getLocVal loc
    nval <- iValSub cval val
    setLocVal loc nval
    return (nval)

executeENeg :: Exp -> Interpreter IVal
executeENeg e = do
    val <- executeExp e
    case val of
        IInt i -> return $ (IInt (-i))
        IFloat f -> return $ (IFloat (-f))
        _ -> throwError ("Invalid value to negate: " ++ (show val))


executeExp :: Exp -> Interpreter IVal
executeExp e = case e of
    EAss var exp -> executeEAss var exp
    EEPlus var exp -> executeEEPlus var exp
    EEMinus var exp -> executeEEMinus var exp
    EEq e1 e2 -> executeEEq e1 e2
    ELt e1 e2 -> executeELt e1 e2
    EGt e1 e2 -> executeELt e2 e1
    EAdd e1 e2 -> executeEAdd e1 e2
    ENeg exp -> executeENeg exp
    EStr str -> return $ IString str
    EInt i   -> return $ IInt i
    EFloat f -> return $ IFloat f
    EBool b  -> return $ IBool (parseTokenBool b)
    EVar var -> do
        loc <- getVarLoc var
        val <- getLocVal loc
        return $ val
    Call func exps -> do
        (IFun f) <- getFun func
        vals <- mapM executeExp exps
        f vals
    _ -> throwError ("Not implemented: " ++ (show e))

executeSIf :: Exp -> Stm -> Interpreter (IEnv, IJump)
executeSIf exp stm = do
    env <- ask
    val <- local (const env) $ executeExp exp
    case val of 
        IBool b -> do
            if b then
                executeStatement stm
            else
                return (env, INothing)
        _ -> throwError ("If got non-bool expression: " ++ (show exp)) 

executeSIfElse :: Exp -> Stm -> Stm -> Interpreter (IEnv, IJump)
executeSIfElse exp stmt stmf = do
    env <- ask
    val <- local (const env) $ executeExp exp
    case val of 
        IBool b -> do
            if b then
                executeStatement stmt
            else
                executeStatement stmf
        _ -> throwError ("If got non-bool expression: " ++ (show exp)) 

executeSWhile :: Exp -> Stm -> Interpreter (IEnv, IJump)
executeSWhile exp stm = do
    env <- ask
    val <- local (const env) $ executeExp exp
    case val of 
        IBool b -> do
            if b then do
                (env1, val) <- local (const env) $ executeStatement stm
                case val of
                    INothing -> local (const env1) $ executeSWhile exp stm
                    IContinue -> local (const env1) $ executeSWhile exp stm
                    IBreak -> return (env1, INothing)
                    IReturn v -> return (env1, val)
            else
                return (env, INothing)
        _ -> throwError ("While got non-bool expression: " ++ (show exp)) 

executeStatement :: Stm -> Interpreter (IEnv, IJump)
executeStatement s = do
    env <- ask
    case s of
        SDecl var -> do
            pvars <- parseVar var
            env1 <- local (const env) $ bindValues pvars
            return $ (env1, INothing)
        SExp e -> do
            val <- executeExp e
            return (env, INothing)
        SIf exp stm -> executeSIf exp stm
        SIfElse exp stmt stmf -> executeSIfElse exp stmt stmf
        SBlock stms -> executeStatements stms
        SWhile exp stm -> executeSWhile exp stm
        SReturnOne exp -> do
            val <- executeExp exp
            return (env, IReturn val)
        _ -> throwError ("executeStatement: Not implemented: " ++ (show s))


executeStatements :: [Stm] -> Interpreter (IEnv, IJump)
executeStatements [] = do
    env <- ask
    return $ (env, INothing)
executeStatements (s:ss) = do
    (env, jump) <- executeStatement s
    case jump of
      INothing -> do
          retval <- local (const env) $ executeStatements ss
          return retval
      _ -> return $ (env, jump)


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
    liftIO $ putStrLn (show store)
    return ()    


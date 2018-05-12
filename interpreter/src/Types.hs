module Types where

import Data.Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import AbsGrammar
import PrintGrammar

import ErrM

data TType
    = TInt
    | TFloat
    | TStr
    | TBool
    | TDict (TType, TType)
    | TStruct [(TVar,TType)]
    | TArray TType
    | Null
    deriving (Show, Eq, Ord)

type TVar   = Ident
type TFName = Ident
type TSName = Ident

newtype TFun = TFun (TypeChecker TType)

type TEnvVar = Map TVar TType
type TEnvFunc = Map TFName ([TType], TFun, TType)
type TEnvStruct = Map TSName TType
type TEnv = (TEnvVar, TEnvFunc, TEnvStruct)

type TResult = ExceptT String IO
type TypeChecker = ReaderT TEnv TResult

tSetFun :: TFName -> [TType] -> TType -> TFun -> TypeChecker TEnv
tSetFun fname types returnType fun = do
    (envVar, envFun, envStruct) <- ask
    return (envVar, insert fname (types, fun, returnType) envFun, envStruct)

typeToTType :: Type -> TType
typeToTType AbsGrammar.TInt = Types.TInt
typeToTType AbsGrammar.TFloat = Types.TFloat
typeToTType AbsGrammar.TStr = Types.TStr
typeToTType AbsGrammar.TBool = Types.TBool

checkTypes :: [TType] -> [TType] -> TypeChecker ()
checkTypes [] [] = return ()
checkTypes (a:as) (b:bs) = if a == b then checkTypes as bs else throwError "no i chuj, nie pykło"

tVarToType :: Var -> TType
tVarToType _ = Types.TFloat
tVarsToTypes :: [Var] -> [TType]
tVarsToTypes _ = []

tStatements :: [Stm] -> TypeChecker TType
tStatements _ = return $ Types.TInt

tDFunction :: Function -> TypeChecker TEnv
tDFunction (FunOne fname fvars rtype stms) = do
    env <- ask
    let ftypes = tVarsToTypes fvars
    let rttype = typeToTType rtype
    let func = do
        env1 <- local (const env) $ tSetFun fname ftypes rttype (TFun func)
        valType <- local (const env1) $ tStatements stms
        if rttype == valType then
            return rttype
        else
            throwError ("Types mismatch: " ++ (show rttype) ++ (show valType))
    envS <- local (const env) $ tSetFun fname ftypes rttype (TFun func)
    return envS

tDFunction (FunNone fname fvars stms) = do
    env <- ask
    let ftypes = tVarsToTypes fvars
    let func = do
        env1 <- local (const env) $ tSetFun fname ftypes Null (TFun func)
        valType <- local (const env1) $ tStatements stms
        if Null == valType then
            return Null
        else
            throwError ("Types mismatch: want Null, but got " ++ (show valType))
    envS <- local (const env) $ tSetFun fname ftypes Null (TFun func)
    return envS

tDFunction f = do
    env <- ask
    liftIO $ putStrLn ("tDFunction, not implemented: " ++ (show f))
    return env

tDStruct :: Struct -> TypeChecker TEnv
tDStruct (IStruct name vars) = do
    env <- ask
    liftIO $ putStrLn ("tDStruct not implemented: " ++ (show name) ++ " -> " ++ (show vars))
    return env

tDVar :: Var -> TypeChecker TEnv
tDVar (DVarOnly vo) = tDVarOnly vo
tDVar (DVarExpr vr) = tDVarExpr vr

tDVarOnly :: VarOnly -> TypeChecker TEnv
tDVarOnly vo = do
    env <- ask
    liftIO $ putStrLn ("tDVarOnly not implemented: " ++ (show vo))
    return env

tDVarExpr :: VarExpr -> TypeChecker TEnv
tDVarExpr vr = do
    env <- ask
    liftIO $ putStrLn ("tDVarExpr not implemented: " ++ (show vr))
    return env

tDeclaration :: Decl -> TypeChecker TEnv
tDeclaration (DFunction f) = tDFunction f
tDeclaration (DStruct s) = tDStruct s
tDeclaration (DVar v) = tDVar v

tDeclarations :: [Decl] -> TypeChecker TEnv
tDeclarations [] = ask
tDeclarations (d:ds) = do
    env <- ask
    env1 <- local (const env) $ tDeclaration d
    env2 <- local (const env1) $ tDeclarations ds
    return env2

tProgram :: Program -> TypeChecker ()
tProgram (Prog declarations) = do
    env <- tDeclarations declarations
    return ()

typesAnalyze :: Program -> TResult ()
typesAnalyze program = do
    runReaderT (tProgram program) (empty, empty, empty)
    return ()


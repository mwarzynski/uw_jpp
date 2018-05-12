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
    | TReturn TSName
    | Null
    deriving (Show, Eq, Ord)

type TVar   = Ident
type TFName = Ident
type TSName = Ident

newtype TFun = TFun (TypeChecker ())

type TEnvVar = Map TVar TType
type TEnvFunc = Map TFName ([TType], TFun, TType)
type TEnvStruct = Map TSName TType
type TEnv = (TType, TEnvVar, TEnvFunc, TEnvStruct)

type TResult = ExceptT String IO
type TypeChecker = ReaderT TEnv TResult

tSetFun :: TFName -> [TType] -> TType -> TFun -> TypeChecker TEnv
tSetFun fname types returnType fun = do
    (returnType, envVar, envFun, envStruct) <- ask
    return (returnType, envVar, insert fname (types, fun, returnType) envFun, envStruct)

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

tStatement :: Stm -> TypeChecker TEnv
tStatement (SFunc func) = do
    env <- ask
    (env1, (TFun fun)) <- local (const env) $ tDFunction func
    fun
    return env1
tStatement s = throwError ("tStatement: Not implemented: " ++ (show s))

tStatements :: [Stm] -> TypeChecker TEnv
tStatements [] = do
    env <- ask
    return env
tStatements (s:ss) = do
    t <- tStatement s
    tStatements ss

tDFunction :: Function -> TypeChecker (TEnv, TFun)
tDFunction (FunOne fname fvars rtype stms) = do
    env <- ask
    let ftypes = tVarsToTypes fvars
    let rttype = typeToTType rtype
    let func = do
        (_, a, b, c) <- local (const env) $ tSetFun fname ftypes rttype (TFun func)
        let env1 = (rttype, a, b, c)
        local (const env1) $ tStatements stms
        return ()
    let tfun = (TFun func)
    envS <- local (const env) $ tSetFun fname ftypes rttype tfun
    return (envS, tfun)
tDFunction (FunNone fname fvars stms) = do
    env <- ask
    let ftypes = tVarsToTypes fvars
    let func = do
        (_, a, b, c) <- local (const env) $ tSetFun fname ftypes Null (TFun func)
        let env1 = (Null, a, b, c)
        local (const env1) $ tStatements stms
        return ()
    let tfun = (TFun func)
    envS <- local (const env) $ tSetFun fname ftypes Null tfun
    return (envS, tfun)
tDFunction (FunStr fname fvars rstruct stms) = do
    env <- ask
    let (_, _, _, sEnv) = env
    if (Data.Map.lookup rstruct sEnv) /= Nothing then do
        let ftypes = tVarsToTypes fvars
        let func = do
            (_, a, b, c) <- local (const env) $ tSetFun fname ftypes (TReturn rstruct) (TFun func)
            let env1 = ((TReturn rstruct), a, b, c)
            local (const env1) $ tStatements stms
            return ()
        let tfun = (TFun func)
        envS <- local (const env) $ tSetFun fname ftypes Null tfun
        return (envS, tfun)
    else throwError ("Struct " ++ (show rstruct) ++ " does not exist")

tDStruct :: Struct -> TypeChecker TEnv
tDStruct (IStruct name vars) = throwError ("tDStruct not implemented: " ++ (show name) ++ " -> " ++ (show vars))

tDVar :: Var -> TypeChecker TEnv
tDVar (DVarOnly vo) = tDVarOnly vo
tDVar (DVarExpr vr) = tDVarExpr vr

tDVarOnly :: VarOnly -> TypeChecker TEnv
tDVarOnly vo = throwError ("tDVarOnly not implemented: " ++ (show vo))

tDVarExpr :: VarExpr -> TypeChecker TEnv
tDVarExpr vr = throwError ("tDVarExpr not implemented: " ++ (show vr))

tDeclaration :: Decl -> TypeChecker TEnv
tDeclaration (DFunction f) = do
    (env, _) <- tDFunction f
    return env
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
    let (_, _, funcEnv, _) = env
    checkFuncs (Data.Map.toList funcEnv)
    return ()
    where checkFuncs :: [(TFName, ([TType], TFun, TType))] -> TypeChecker ()
          checkFuncs [] = return $ ()
          checkFuncs (v:vs) = do
              t <- checkFunc (snd v)
              checkFuncs vs
          checkFunc (_, (TFun func), _) = do
              t <- func
              return $ t

typesAnalyze :: Program -> TResult ()
typesAnalyze program = do
    runReaderT (tProgram program) (Null, empty, empty, empty)
    return ()


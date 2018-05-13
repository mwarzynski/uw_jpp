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

tSetVar :: TVar -> TType -> TypeChecker TEnv
tSetVar var t = do
    (rtype, envVar, envFunc, envStruct) <- ask
    return (rtype, insert var t envVar, envFunc, envStruct)

tGetVarType :: TVar -> TypeChecker TType
tGetVarType var = do
    (_, envVar, _, _) <- ask
    if (Data.Map.lookup var envVar) /= Nothing then do
        let t = envVar ! var
        return t
    else throwError ("Variable was not declared before: " ++ (show var))

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

tVarToType :: Var -> TypeChecker TType
tVarToType _ = return Null

tVarsToTypes :: [Var] -> TypeChecker [TType]
tVarsToTypes [] = return $ []
tVarsToTypes (v:vs) = do
    t <- tVarToType v
    ts <- tVarsToTypes vs
    return $ [t] ++ ts

tExpToType :: Exp -> TypeChecker TType
tExpToType exp = do
    t <- tExp exp
    return t

tExpsToTypes :: [Exp] -> TypeChecker [TType]
tExpsToTypes [] = return $ []
tExpsToTypes (e:es) = do
    t <- tExpToType e
    ts <- tExpsToTypes es
    return $ [t] ++ ts

tVarOnly :: VarOnly -> TypeChecker TEnv
tVarOnly vo = case vo of
    Dec name vtype -> do
        env <- ask
        env1 <- local (const env) $ tSetVar name (typeToTType vtype)
        return env1
    -- DecStruct name stype
    DecDict name ktype vtype -> do
        let kt = typeToTType ktype
        let vt = typeToTType vtype
        env <- tSetVar name (TDict (kt, vt))
        return env
    DecArrMul name itype length -> do
        env <- ask
        let ttype = typeToTType itype
        env1 <- local (const env) $ tSetVar name (TArray ttype)
        return env1
    _ -> throwError (show vo)
tVarExpr :: VarExpr -> TypeChecker TEnv
tVarExpr vr = case vr of
    DecSet name vtype exp -> do
        env <- ask
        t <- tExp exp
        let expectedT = typeToTType vtype
        if t /= expectedT then throwError ("Invalid expression type for declared variable: " ++ show(name) ++ ", want: " ++ (show expectedT) ++ ", got: " ++ (show t))
        else do
            env1 <- local (const env) $ tSetVar name t
            return env1
    -- DecArr name itype exps ->
    -- DecAtructSet name sname exp ->
    -- DecArrMulInit name itype length exp ->

tVar :: Var -> TypeChecker TEnv
tVar v = case v of
    DVarOnly vo -> tVarOnly vo
    DVarExpr vr -> tVarExpr vr

tExp :: Exp -> TypeChecker TType
tExp (EAss name exp) = do
    t <- tExp exp
    expectedT <- tGetVarType name
    if t == expectedT then
        return Null
    else throwError ("Invalid assignment to " ++ (show name) ++ ", want: " ++ (show expectedT) ++ ", got: " ++ (show t))
tExp (EAssArr name indexExp valExp) = do
    indexType <- tExp indexExp
    valType <- tExp valExp
    arrType <- tGetVarType name
    case arrType of
      TArray aType -> if indexType /= Types.TInt then
                                    throwError ("Invalid index type: " ++ (show indexType))
                                else if valType /= aType then
                                    throwError ("Invalid value type for " ++ (show name) ++ ", want: " ++ (show aType) ++ ", got: " ++ (show valType))
                                else return Null
      TDict (kType, vType) -> if indexType /= kType then
                                                    throwError ("Assigning to " ++ (show name) ++ " - invalid key type: want: " ++ (show kType) ++ ", got: " ++ (show indexType))
                                else if valType /= vType then
                                    throwError ("Assigning to " ++ (show name) ++ " - invalid value type: want: " ++ (show vType) ++ ", got: " ++ (show valType))
                                else return Null
      _ -> throwError ("Invalid index type for " ++ (show name) ++ ", want: int, got: " ++ (show indexExp))
tExp (EAssStr name attrName value) = throwError ("EAssStr: Not implemented")
tExp (EEPlus name exp) = do
    env <- ask
    t <- tGetVarType name
    valType <- tExp exp
    case t of
      Types.TInt -> case valType of
        Types.TInt -> return Null
        _ -> throwError ("Invalid value for += operation (want Int) on " ++ (show name))
      Types.TFloat -> case valType of
        Types.TFloat -> return Null
        _ -> throwError ("Invalid value for += operation (want Float) on " ++ (show name))
      _ -> throwError ("Invalid variable to execute += on.")
tExp (EEMinus name exp) = do
    env <- ask
    t <- tGetVarType name
    valType <- tExp exp
    case t of
      Types.TInt -> case valType of
        Types.TInt -> return Null
        _ -> throwError ("Invalid value for -= operation (want Int) on " ++ (show name))
      Types.TFloat -> case valType of
        Types.TFloat -> return Null
        _ -> throwError ("Invalid value for -= operation (want Float) on " ++ (show name))
      _ -> throwError ("Invalid variable to execute -= on.")
tExp (ElOr b1 b2) = do
    t1 <- tExp b1
    t2 <- tExp b2
    if t1 == Types.TBool && t2 == Types.TBool then
        return Types.TBool
    else throwError ("|| needs boolean values, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (ElAnd e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == Types.TBool && t2 == Types.TBool then
        return Types.TBool
    else throwError ("&& needs boolean values, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (EEq e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 then
        return Types.TBool
    else throwError ("== needs the same types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (ENEq e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 then
        return Types.TBool
    else throwError ("!= needs the same types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (ELt e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 then
        return Types.TBool
    else throwError ("< needs the same types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (ELtE e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 then
        return Types.TBool
    else throwError ("<= needs the same types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (ELt2 e1 e2 e3) = do
    t1 <- tExp e1
    t2 <- tExp e2
    t3 <- tExp e3
    if t1 == t2 && t2 == t3 then
        return Types.TBool
    else throwError ("a < b < c needs the same types, got: " ++ (show t1) ++ ", " ++ (show t2) ++ ", " ++ (show t3))
tExp (EGt e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 then
        return Types.TBool
    else throwError ("> needs the same types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (EGtE e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 then
        return Types.TBool
    else throwError (">= needs the same types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (EGt2 e1 e2 e3) = do
    t1 <- tExp e1
    t2 <- tExp e2
    t3 <- tExp e3
    if t1 == t2 && t2 == t3 then
        return Types.TBool
    else throwError ("a > b > c needs the same types, got: " ++ (show t1) ++ ", " ++ (show t2) ++ ", " ++ (show t3))
tExp (Call fname exps) = do
    (_, _, funcEnv, _) <- ask
    return Null
tExp (EStr s) = return Types.TStr
tExp (EInt i) = return Types.TInt
tExp (EFloat f) = return Types.TFloat
tExp (EBool b) = return Types.TBool
tExp e = throwError ("tExp: Not implemented: " ++ (show e))


tStatement :: Stm -> TypeChecker TEnv
tStatement (SFunc func) = do
    env <- ask
    (env1, (TFun fun)) <- local (const env) $ tDFunction func
    fun
    return env1
tStatement (SDecl v) = do
    env <- ask
    env1 <- local (const env) $ tVar v
    return env1
tStatement (SExp exp) = do
    env <- ask
    t <- tExp exp
    return env
tStatement (SIf exp stm) = do
    b <- tExp exp
    if b == Types.TBool then do
        env <- tStatement stm
        return env
    else throwError ("If requires boolean value, got: " ++ (show b))
tStatement (SBlock stms) = tStatements stms
tStatement s = throwError ("tStatement: Not implemented: " ++ (show s))

tStatements :: [Stm] -> TypeChecker TEnv
tStatements [] = ask
tStatements (s:ss) = do
    env <- tStatement s
    env1 <- local (const env) $ tStatements ss
    return env1

tDFunction :: Function -> TypeChecker (TEnv, TFun)
tDFunction (FunOne fname fvars rtype stms) = do
    env <- ask
    ftypes <- tVarsToTypes fvars
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
    ftypes <- tVarsToTypes fvars
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
        ftypes <- tVarsToTypes fvars
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


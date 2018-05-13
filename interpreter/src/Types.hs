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
    | TStruct (Map TVar TType)
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

tGetStructType :: TSName -> TypeChecker TType
tGetStructType name = do
    (_, _, _, envStruct) <- ask
    if (Data.Map.lookup name envStruct) /= Nothing then do
        let s = envStruct ! name
        return s
    else throwError ("Struct " ++ (show name) ++ " was not defined.")

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
    _ -> throwError ("tVarOnly not implemented: " ++ (show vo))

tExpsHaveType :: [Exp] -> TType -> TypeChecker ()
tExpsHaveType [] _ = return ()
tExpsHaveType (e:es) t = do
    et <- tExp e
    if et == t then (tExpsHaveType es t)
    else throwError ("Checking list of exps, invalid type: want: " ++ (show t) ++ ", got: " ++ (show et))

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
    DecArr name itype exps -> do
        env <- ask
        let it = typeToTType itype
        tExpsHaveType exps it
        env1 <- local (const env) $ tSetVar name (TArray it)
        return env1
    -- DecStructSet name sname exp ->
    DecArrMulInit name itype _ exp -> do
        env <- ask
        let it = typeToTType itype
        expT <- tExp exp
        if it == expT then do
            env1 <- local (const env) $ tSetVar name (TArray it)
            return env1
        else throwError ("Array declaration, invalid expression type: want: " ++ (show it) ++ ", got: " ++ (show expT))
    _ -> throwError ("tVarExpr not implemented " ++ (show vr))

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
tExp (EAdd e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 && (t1 == Types.TInt || t2 == Types.TFloat || t2 == Types.TStr) then
        return t1
    else throwError ("+ needs the valid types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (ESub e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 && (t1 == Types.TInt || t1 == Types.TFloat) then
        return t1
    else throwError ("- needs the valid types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (EMul e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 && (t1 == Types.TInt || t1 == Types.TFloat) then
        return t1
    else throwError ("* needs the valid types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (EDiv e1 e2) = do
    t1 <- tExp e1
    t2 <- tExp e2
    if t1 == t2 && (t1 == Types.TInt || t1 == Types.TFloat) then
        return t1
    else throwError ("/ needs the valid types, got: " ++ (show t1) ++ " and " ++ (show t2))
tExp (Call fname exps) = do
    -- TODO tExp Call
    (_, _, funcEnv, _) <- ask
    return Null
tExp (EVarArr name index) = do
    ar <- tGetVarType name
    indexType <- tExp index
    case ar of
      TArray aType -> if indexType == Types.TInt then return aType
                      else throwError ("Accessing array " ++ (show name) ++ " with invalid index type " ++ (show indexType))
      TDict (keyType, valType) -> if indexType == keyType then return valType
                                  else throwError ("Accessing dict " ++ (show name) ++ " with invalid key type: want: " ++ (show keyType) ++ ", got: " ++ (show indexType))
      _ -> throwError ("Accessing invalid variable type: " ++ (show name))
tExp (EStrAtt name attr) = do
    -- TODO check this shit
    t <- tGetVarType name
    case t of
      TStruct attrsMap -> do
          if (Data.Map.lookup attr attrsMap) /= Nothing then
              return $ attrsMap ! attr
          else throwError ("Struct variable " ++ (show name) ++ " does not have attribute: " ++ (show attr))
      _ -> throwError ("Variable " ++ (show name) ++ "is not a struct.")
tExp (EPPos name) = do
    t <- tGetVarType name
    case t of
      Types.TInt -> return Types.TInt
      Types.TFloat -> return Types.TFloat
      _ -> throwError ("Could not execute ++ on type: " ++ (show t) ++ " for variable " ++ (show name))
tExp (EMMin name) = do
    t <- tGetVarType name
    case t of
      Types.TInt -> return Types.TInt
      Types.TFloat -> return Types.TFloat
      _ -> throwError ("Could not execute ++ on type: " ++ (show t) ++ " for variable " ++ (show name))
tExp (EBNeg exp) = do
    t <- tExp exp
    if t == Types.TBool then return Types.TBool
    else throwError ("Could not negate non-boolean value, got: " ++ (show t))
tExp (ENeg exp) = do
    t <- tExp exp
    case t of
      Types.TInt -> return Types.TInt
      Types.TFloat -> return Types.TFloat
      _ -> throwError ("Could not negate value " ++ (show t))
tExp (EPos exp) = do
    t <- tExp exp
    case t of
      Types.TInt -> return Types.TInt
      Types.TFloat -> return Types.TFloat
      _ -> throwError ("Could not positive value " ++ (show t))
tExp (EVar name) = do
    t <- tGetVarType name
    return t
tExp (EStr s) = return Types.TStr
tExp (EInt i) = return Types.TInt
tExp (EFloat f) = return Types.TFloat
tExp (EBool b) = return Types.TBool


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
tStatement (SBlock stms) = tStatements stms
tStatement (SWhile exp stm) = do
    t <- tExp exp
    if t == Types.TBool then
        tStatement stm
    else throwError ("Invalid while condition type " ++ (show t))
tStatement (SForD var expCheck expFinal stm) = do
    env <- ask
    env1 <- local (const env) $ tVar var
    tCheck <- local (const env1) $ tExp expCheck
    e <- local (const env1) $ tExp expFinal
    if tCheck == Types.TBool then do
        env2 <- local (const env1) $ tStatement stm
        return env
    else throwError ("For condition must be boolean, got " ++ (show tCheck))
tStatement (SForE expBefore expCheck expFinal stm) = do
    env <- ask
    e <- local (const env) $ tExp expBefore
    tCheck <- local (const env) $ tExp expCheck
    e <- local (const env) $ tExp expFinal
    if tCheck == Types.TBool then do
        env2 <- local (const env) $ tStatement stm
        return env
    else throwError ("For condition must be boolean, got " ++ (show tCheck))
tStatement (SIf exp stm) = do
    b <- tExp exp
    if b == Types.TBool then do
        env <- tStatement stm
        return env
    else throwError ("If statement requires boolean value, got: " ++ (show b))
tStatement (SIfElse exp stmok stmelse) = do
    env <- ask
    b <- tExp exp
    if b == Types.TBool then do
        env1 <- local (const env) $ tStatement stmok
        env1 <- local (const env) $ tStatement stmelse
        return env
    else throwError ("If statement requires boolean value, got: " ++ (show b))
tStatement (SReturnOne exp) = do
    env <- ask
    let (expT, a, b, c) = env
    t <- tExp exp
    if t == expT then return env
    else if expT == Null then return (t, a, b, c)
    else throwError ("Invalid return value type: want: " ++ (show expT) ++ ", got: " ++ (show t))
tStatement SReturn = do
    env <- ask
    let (expT, a, b, c) = env
    if expT == Null then
        return (expT, a, b, c)
    else throwError ("Invalid return value, function does not return value.")
tStatement SJContinue = ask
tStatement SJBreak = ask

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
    let (_, vEnv, funcEnv, sEnv) = env
    let fEnv = (Null, vEnv, funcEnv, sEnv)
    let func = do
        fEnv2 <- local (const fEnv) $ tSetFun fname ftypes rttype (TFun func)
        (returnType, _, _, _) <- local (const fEnv2) $ tStatements stms
        if returnType == rttype then return ()
        else throwError ("Function " ++ (show fname) ++ " returned invalid type, want: " ++ (show rttype) ++ ", got: " ++ (show returnType))
    let tfun = (TFun func)
    envS <- local (const env) $ tSetFun fname ftypes rttype tfun
    return (envS, tfun)
tDFunction (FunNone fname fvars stms) = do
    env <- ask
    ftypes <- tVarsToTypes fvars
    let (_, vEnv, funcEnv, sEnv) = env
    let fEnv = (Null, vEnv, funcEnv, sEnv)
    let func = do
        fEnv2 <- local (const fEnv) $ tSetFun fname ftypes Null (TFun func)
        (returnType, _, _, _) <- local (const fEnv2) $ tStatements stms
        if returnType == Null then return ()
        else throwError ("Function " ++ (show fname) ++ " instead of Null returned " ++ (show returnType))
    let tfun = (TFun func)
    envS <- local (const env) $ tSetFun fname ftypes Null tfun
    return (envS, tfun)
tDFunction (FunStr fname fvars rstruct stms) = do
    env <- ask
    rttype <- tGetStructType rstruct
    let (_, vEnv, funcEnv, sEnv) = env
    if (Data.Map.lookup rstruct sEnv) /= Nothing then do
        ftypes <- tVarsToTypes fvars
        let fEnv = (Null, vEnv, funcEnv, sEnv)
        let func = do
            ffEnv <- local (const fEnv) $ tSetFun fname ftypes (TReturn rstruct) (TFun func)
            (returnType, _, _, _) <- local (const ffEnv) $ tStatements stms
            if returnType == rttype then return ()
            else throwError ("Function " ++ (show fname) ++ " returned invalid struct, want: " ++ (show rstruct))
        let tfun = (TFun func)
        envS <- local (const env) $ tSetFun fname ftypes (TReturn rstruct) tfun
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


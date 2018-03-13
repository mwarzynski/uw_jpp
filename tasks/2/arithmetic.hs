import Data.Map

data Exp 
  = EInt Integer         -- constant
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- variable
  | ELet String Exp Exp  -- let var = e1 in e2 

-- A)
-- Write instances of Eq, Show for Exp
--  Exp
ews :: Exp -> Map String Integer -> Integer
ews (EInt e) store = e
ews (EAdd e1 e2) store = (ews e1 store) + (ews e2 store)
ews (ESub e1 e2) store = (ews e1 store) - (ews e2 store)
ews (EMul e1 e2) store = (ews e1 store) * (ews e2 store)
ews (EVar x) store = case Data.Map.lookup x store of 
                         Just a -> a
                         Nothing -> error("ews::Map.lookup::error element '" ++ x ++ "' not found")
ews (ELet x e1 e2) (store) = ews e2 (Data.Map.insert x (ews e1 store) store)

eval :: Exp -> Integer
eval e = ews e Data.Map.empty

instance Eq Exp where
    e1 == e2 = (eval(e1) == eval(e2))

instance Show Exp where
    show (EInt e) = show e
    show (EAdd e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (ESub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (EMul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
    show (EVar x) = x
    show (ELet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in (" ++ show e2 ++ ")"

-- B)
-- implement Num instance for Exp
-- testExp2 :: Exp
-- testExp2 = (2 + 2) * 3
-- (methods abs and signum might be undefined)
instance Num Exp where
    e1 + e2 = EAdd e1 e2
    e1 * e2 = EMul e1 e2
    e1 - e2 = ESub e1 e2
    fromInteger n = EInt n


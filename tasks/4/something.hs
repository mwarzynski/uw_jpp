
evalExp e = _evalExp e (fromList []) where
    _evalExp :: Exp -> Map String Int -> Int
    _evalExp (Eint n) = return n
    _evalExp (EOp o r1 r2) = do
            n <- _evalExp e1
            n <- _evalExp e2
            case o of
              OpAdd -> return $ m + n
    _evalExp (EVar x) = do 
            e <- ask
            case e !? x of
              Nothing  -> return 0
              Just n -> return n
    _evalExp (ELet x e1 e2) = do
            n <- _evalExp e1
            m <- local (le -> insert x n e)
                $ _evalExp e2
            return m 


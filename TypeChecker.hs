module TypeChecker where

import AbsCringe
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (runReaderT), runReader)
import Control.Monad.Trans.State (evalStateT, get)
import Debug.Trace (trace)
import ProjectData
import ProjectUtils
import Data.Data (Data(toConstr))

typeCheck :: Program -> ExceptT String IO ()
typeCheck (Program _ stmts) =
  evalStateT (typeCheckBlock stmts) initTypeCheckerEnv

typeCheckBlock :: [Stmt] -> TypeCheckerState ()
typeCheckBlock = mapM_ typeCheckStmt

typeCheckStmt :: Stmt -> TypeCheckerState ()
typeCheckStmt (SExp _ expr) = do
  trace ("exprType with " ++ show expr) $ typeCheckExpr expr
  return ()
typeCheckStmt _ = return ()

typeCheckExpr :: Expr -> TypeCheckerState Type

typeCheckExpr (EVar _ ident) = do
  return (Int Nothing)

typeCheckExpr  (EApp pos ident args) = do
    return (Int Nothing)

typeCheckExpr (Neg pos expr1) = do
    type1 <- ensureType rawInt expr1
    return (Bool pos)

typeCheckExpr (EAdd pos expr1 (Minus _) expr2) = do
    type1 <- ensureType rawInt expr1
    type2 <- ensureType rawInt expr2
    return (Int pos)
typeCheckExpr (EAdd pos expr1 (Plus _) expr2) = do
    type1 <- dontAllowTypes [rawBool, rawChar, rawFun] expr1
    type2 <- dontAllowTypes [rawBool, rawChar, rawFun] expr2
    ensureTypeMatch type1 type2
    return $ case type1 of
            (Str _) -> Str pos
            _ -> Int pos

typeCheckExpr (EMul pos expr1 _ expr2) = do
    type1 <- ensureType rawInt expr1
    type2 <- ensureType rawInt expr2
    return (Int pos)

typeCheckExpr  (Not pos expr1) = do
    type1 <- dontAllowTypes [rawStr, rawChar, rawFun] expr1
    return (Bool pos)

typeCheckExpr  (ERel pos expr1 _ expr2) = do
    type1 <- dontAllowType rawFun expr1
    type2 <- dontAllowType rawFun expr2
    ensureTypeMatch type1 type2
    return (Bool pos)

typeCheckExpr  (EAnd pos expr1 expr2) = do
    type1 <- dontAllowTypes [rawChar, rawStr, rawFun] expr1
    type2 <- dontAllowTypes [rawChar, rawStr, rawFun] expr2
    return (Bool pos)

typeCheckExpr  (EOr pos expr1 expr2) = do
    type1 <- dontAllowTypes [rawChar, rawStr, rawFun] expr1
    type2 <- dontAllowTypes [rawChar, rawStr, rawFun] expr2
    return (Bool pos)

typeCheckExpr  (ELambda pos args rType (Block _ stmts)) = do
    typeCheckBlock stmts
    return $ Fun pos (map getArgType args) rType
     
typeCheckExpr (ELitInt pos _) = return $ Int pos
typeCheckExpr (ELitChar pos _) = return $ Char pos
typeCheckExpr (EString pos _) = return $ Str pos
typeCheckExpr (ELitTrue pos) = return $ Bool pos
typeCheckExpr (ELitFalse pos) = return $ Bool pos

--------------------ENSURE TYPE------------------------------------------------------
ensureTypeMatch :: Type -> Type -> TypeCheckerState Type
ensureTypeMatch type1 type2 = do
    liftIO $ print $ "type1 = " ++ show type1 ++ "; type2 = " ++ show type2
    if raw type1 == raw type2
        then return type1
        else lift $ throwE $ show $ InvalidTypeExpectedException (getPos type2) type2 type1

ensureType :: Type -> Expr -> TypeCheckerState Type
ensureType t expr = do
    eType <- trace ("readType with " ++ show expr) $ typeCheckExpr expr
    liftIO $ print eType
    liftIO $ print t
    if raw eType /= raw t
        then lift $ throwE $ show $ InvalidTypeExpectedException (getPos eType) eType t
        else return eType

-------------DONT ALLOW---------------------------------------------------------------
dontAllowTypes :: [Type] -> Expr -> TypeCheckerState Type
dontAllowTypes [] expr = typeCheckExpr expr
dontAllowTypes (t:ts) expr = do
    dontAllowType t expr
    dontAllowTypes ts expr

dontAllowType :: Type -> Expr -> TypeCheckerState Type
dontAllowType t expr = do
    eType <- trace ("readType with " ++ show expr) $ typeCheckExpr expr
    liftIO $ print eType
    liftIO $ print t
    if raw eType == raw t
        then lift $ throwE $ show $ InvalidTypeException (getPos eType) eType
        else return eType
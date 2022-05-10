module TypeChecker where

import AbsCringe
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (evalStateT, get, gets, put)
import Data.List
import Debug.Trace (trace)
import ProjectData
import ProjectUtils

typeCheck :: Program -> ExceptT String IO ()
typeCheck (Program _ stmts) =
  evalStateT (typeCheckBlock stmts) initTypeCheckerEnv

typeCheckBlock :: [Stmt] -> TypeCheckerState ()
typeCheckBlock = mapM_ typeCheckStmt

--------------------------------STMT------------------------------------------------------------------------------
typeCheckStmt :: Stmt -> TypeCheckerState ()
typeCheckStmt (Empty _) = return ()

typeCheckStmt (BStmt _ (Block _ stmts)) = do
  env <- get
  localState env (typeCheckBlock stmts)

typeCheckStmt (Decl pos t item) = typeCheckDecl pos t item False

typeCheckStmt (ConstDecl pos t item) = typeCheckDecl pos t item True

typeCheckStmt (Ass pos ident expr) = do
  env <- get
  case getType env ident of
    Nothing -> throwTC $ UndefinedException pos ident
    Just t -> do
      if snd t
        then throwTC $ ImmutableCannotBeChangedException pos ident
        else ensureType (fst t) expr

typeCheckStmt (Incr pos ident) =
  typeCheckStmt (Ass pos ident (EAdd pos (ELitInt pos 1) (Plus pos) (EVar pos ident)))

typeCheckStmt (Decr pos ident) =
  typeCheckStmt (Ass pos ident (EAdd pos (ELitInt pos 1) (Minus pos) (EVar pos ident)))

typeCheckStmt (Ret pos expr) = do
  env <- get
  case expectedReturnType env of
    Nothing -> throwTC $ InvalidReturnException pos
    Just t -> ensureType t expr

typeCheckStmt (VRet pos) = do
  env <- get
  case expectedReturnType env of
    Nothing -> throwTC $ InvalidReturnException pos
    Just t -> ensureTypeMatch pos rawVoid t

typeCheckStmt (Cond pos expr stmt) = do
  dontAllowTypes [rawFun, rawVoid, rawChar, rawStr] expr
  env <- get
  localState env (typeCheckStmt stmt)

typeCheckStmt (CondElse pos expr (Block _ ifBlock) (Block _ elseBlock)) = do
  dontAllowTypes [rawFun, rawVoid, rawChar, rawStr] expr
  typeCheckBlock ifBlock
  typeCheckBlock elseBlock

typeCheckStmt (While pos expr stmt) = do
  dontAllowTypes [rawFun, rawVoid, rawChar, rawStr] expr
  env <- get
  localState (setInsideLoop env True) (typeCheckStmt stmt)

typeCheckStmt (For pos ident from to stmt) = do
  ensureType rawInt from
  ensureType rawInt to
  env <- get
  localState (setType (setInsideLoop env True) ident (Int pos, True)) (typeCheckStmt stmt)

typeCheckStmt (Print pos expr) = do
  dontAllowTypes [rawFun, rawVoid] expr
  return ()

typeCheckStmt (Break pos) = do
  env <- get
  unless (insideLoop env) (throwTC $ InvalidExitException pos)

typeCheckStmt (Continue pos) = do
  env <- get
  unless (insideLoop env) (throwTC $ InvalidSkipException pos)

typeCheckStmt (SExp _ expr) = do
  typeCheckExpr expr
  return ()

-----------------DECL-------------------------------------------------------------------------------
typeCheckDecl :: BNFC'Position -> Type -> Item -> Bool -> TypeCheckerState ()
typeCheckDecl pos t (NoInit _ ident) True = throwTC $ ImmutableNotInitializedException pos ident

typeCheckDecl pos t (NoInit _ ident) False = do
  dontAllowVoid t
  env <- get
  case getType env ident of
    Nothing -> put $ setType env ident (t, False)
    Just _ -> throwTC $ RedeclarationException pos ident

typeCheckDecl pos t (Init _ ident expr) isImmutable = do
  dontAllowVoid t
  eType <- typeCheckExpr expr
  ensureTypeMatch pos t eType
  env <- get
  case getType env ident of
    Nothing -> put $ setType env ident (t, isImmutable)
    Just _ -> throwTC $ RedeclarationException pos ident

---------------EXPR---------------------------------------------------------------------------------------
typeCheckExpr :: Expr -> TypeCheckerState Type
typeCheckExpr (EVar pos ident) = do
  env <- get
  case getType env ident of
    Nothing -> throwTC $ UndefinedException pos ident
    Just t -> return $ fst t

typeCheckExpr (EApp pos ident exprs) = do
  env <- get
  case getType env ident of
    Nothing -> throwTC $ UndefinedException pos ident
    Just (Fun _ args rType, _) -> do
      ensureArgTypes pos args exprs
      return rType
    Just (t, _) -> throwTC $ InvalidFunctionApplicationException pos ident t

typeCheckExpr (Neg pos expr1) = do
  type1 <- ensureType rawInt expr1
  return (Bool pos)

typeCheckExpr (EAdd pos expr1 (Minus _) expr2) = do
  type1 <- ensureType rawInt expr1
  type2 <- ensureType rawInt expr2
  return (Int pos)

typeCheckExpr (EAdd pos expr1 (Plus _) expr2) = do
  type1 <- dontAllowTypes [rawBool, rawChar, rawFun, rawVoid] expr1
  type2 <- dontAllowTypes [rawBool, rawChar, rawFun, rawVoid] expr2
  ensureTypeMatch pos type1 type2
  return $ case type1 of
    (Str _) -> Str pos
    _ -> Int pos

typeCheckExpr (EMul pos expr1 _ expr2) = do
  type1 <- ensureType rawInt expr1
  type2 <- ensureType rawInt expr2
  return (Int pos)

typeCheckExpr (Not pos expr1) = do
  type1 <- dontAllowTypes [rawStr, rawChar, rawFun, rawVoid] expr1
  return (Bool pos)

typeCheckExpr (ERel pos expr1 _ expr2) = do
  type1 <- dontAllowType rawFun expr1
  type2 <- dontAllowType rawFun expr2
  ensureTypeMatch pos type1 type2
  return (Bool pos)

typeCheckExpr (EAnd pos expr1 expr2) = do
  type1 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr1
  type2 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr2
  return (Bool pos)

typeCheckExpr (EOr pos expr1 expr2) = do
  type1 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr1
  type2 <- dontAllowTypes [rawChar, rawStr, rawFun, rawVoid] expr2
  return (Bool pos)

typeCheckExpr (ELambda pos args rType (Block _ stmts)) = do
  dontAllowVoidArgs $ map getArgType args
  ensureUniqueIdents args
  env <- get
  localState
    ( setTypes
        (setExpectedReturnType env $ Just rType)
        (map getArgIdent args)
        (map (makeFalsyTuple . toType . getArgType) args)
    )
    (typeCheckBlock stmts)
  return $ Fun pos (map getArgType args) rType

typeCheckExpr (ELitInt pos _) = return $ Int pos
typeCheckExpr (ELitChar pos _) = return $ Char pos
typeCheckExpr (EString pos _) = return $ Str pos
typeCheckExpr (ELitTrue pos) = return $ Bool pos
typeCheckExpr (ELitFalse pos) = return $ Bool pos

--------------------ENSURE------------------------------------------------------
ensureTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
ensureTypeMatch pos type1 type2 =
  if raw type1 == raw type2
    then return ()
    else throwTC $ InvalidTypeExpectedException pos type2 type1

ensureType :: Type -> Expr -> TypeCheckerState ()
ensureType t expr = do
  eType <- typeCheckExpr expr
  ensureTypeMatch (hasPosition eType) t eType

ensureArgTypes :: BNFC'Position -> [ArgType] -> [Expr] -> TypeCheckerState ()
ensureArgTypes pos ts exprs =
  if length ts /= length exprs
    then throwTC $ InvalidNumberOfParametersException pos
    else zipWithM_ ensureArgType ts exprs

ensureArgType :: ArgType -> Expr -> TypeCheckerState ()
ensureArgType (Val _ t) expr = do
  ensureType t expr

ensureArgType (Ref pos t) expr = do
  case expr of
    (EVar pos ident) -> do
      env <- get
      case getType env ident of
        Nothing -> throwTC $ UndefinedException pos ident
        Just t1 -> if snd t1 then throwTC $ ConstReferenceException pos else ensureTypeMatch (hasPosition $ fst t1) t (fst t1)
    _ -> throwTC $ ReferenceNotVariableException pos

ensureUniqueIdents :: [Arg] -> TypeCheckerState ()
ensureUniqueIdents args =
  case firstDuplicateIndex $ map getArgIdent args of
    Just index -> throwTC $ RedeclarationException (hasPosition . getArgType $ args !! index) (getArgIdent $ args !! index)
    Nothing -> return ()

-------------DONT ALLOW---------------------------------------------------------------
dontAllowTypeMatch :: BNFC'Position -> Type -> Type -> TypeCheckerState ()
dontAllowTypeMatch pos type1 type2 =
  if raw type1 /= raw type2
    then return ()
    else throwTC $ InvalidTypeException pos type2

dontAllowTypes :: [Type] -> Expr -> TypeCheckerState Type
dontAllowTypes ts expr = do
  eType <- typeCheckExpr expr
  mapM_ (dontAllowTypeMatch (hasPosition eType) eType) ts
  return eType

dontAllowType :: Type -> Expr -> TypeCheckerState Type
dontAllowType t expr = do
  eType <- typeCheckExpr expr
  dontAllowTypeMatch (hasPosition eType) t eType
  return t

dontAllowVoidArgs :: [ArgType] -> TypeCheckerState ()
dontAllowVoidArgs = mapM_ (dontAllowVoid . toType)

dontAllowVoid :: Type -> TypeCheckerState ()
dontAllowVoid (Fun _ args _) =
  dontAllowVoidArgs args
  
dontAllowVoid t =
  when (raw t == rawVoid) $ throwTC $ VoidNotAllowedException (hasPosition t)

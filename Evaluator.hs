module Evaluator where
import ProjectData
import Control.Monad.Trans.Except (ExceptT, throwE)
import AbsCringe
import Control.Monad.Trans.State (evalStateT, get, modify, put)
import Control.Monad.IO.Class (MonadIO(liftIO))
import ProjectUtils (throwException)
import Control.Monad (when, replicateM_)
import TmOracle (exprDeepLookup)
import Debug.Trace (trace)

eval :: Program -> ExceptT String IO String
eval (Program _ stmts) = do
    evalStateT (evalBlock stmts) initEvaluatorS
    return "\n\nYour cringe code exited with code 0"

evalBlock :: [Stmt] -> EvaluatorState ()
evalBlock stmts = do
    env <- get
    localEnv env (mapM_ evalStmt stmts)

--------------------------------STMT------------------------------------------------------------------------------
evalStmt :: Stmt -> EvaluatorState ()
evalStmt (Empty _) = return ()

evalStmt (BStmt _ (Block _ stmts)) = evalBlock stmts

evalStmt (Decl pos t (NoInit _ ident)) = do
    st <- get
    put $ allocValue st ident (rndInit t)

evalStmt (Decl pos t (Init _ ident expr)) = do
    val1 <- evalExpr expr
    st <- get
    put $ allocValue st ident val1

evalStmt (ConstDecl pos t item) = evalStmt(Decl pos t item)

evalStmt (Ass pos ident expr) = do
    st <- get
    val1 <- evalExpr expr
    put $ setValue st ident val1

evalStmt (Incr pos ident) =
    evalStmt(Ass pos ident (EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)))

evalStmt (Decr pos ident) =
    evalStmt(Ass pos ident (EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)))

evalStmt (Ret pos expr) = return ()

evalStmt (VRet pos) = return ()

evalStmt (Cond pos expr stmt) = do
    val1 <- evalExpr expr
    st <- get
    when (castBool val1) $ localEnv st (evalStmt stmt)

evalStmt (CondElse pos expr (Block _ ifBlock) (Block _ elseBlock)) = do
    val1 <- evalExpr expr
    if castBool val1
        then evalBlock ifBlock
        else evalBlock elseBlock

evalStmt loop@(While pos expr stmt) = do
    val1 <- evalExpr expr
    st <- get
    when (castBool val1) $ localEnv st (evalStmt stmt) >> evalStmt loop

evalStmt (For pos ident from to stmt) = do
    val1 <- evalExpr from
    val2 <- evalExpr to
    st <- get
    let loops = fromIntegral $ castInteger val2 - castInteger val1 + 1
    localEnv (allocValue st ident val1) (replicateM_ loops (evalFor ident stmt))

evalStmt (Print pos expr) = do
    value <- evalExpr expr
    liftIO $ putStr $ show value

evalStmt (PrintLn pos expr) = do
    value <- evalExpr expr
    liftIO $ print value

evalStmt (Break pos) = return ()

evalStmt (Continue pos) = return ()

evalStmt (SExp _ expr) = do
  evalExpr expr
  return ()

-----------FOR--------------------------------------------------------------------------------------------
evalFor :: Ident -> Stmt -> EvaluatorState ()
evalFor ident stmt = do
    st <- get  
    localEnv st (evalStmt stmt) 
    evalStmt (Incr Nothing ident)

---------------EXPR---------------------------------------------------------------------------------------
evalExpr :: Expr -> EvaluatorState Value
evalExpr (EVar pos ident) = do
    st <- get
    case getValue st ident of
      Nothing -> throwException $ GenericRuntimeException pos
      Just v -> return v

evalExpr (EApp pos ident exprs) = do
    st <- get
    case getValue st ident of
      Nothing -> throwException $ GenericRuntimeException pos
      Just (FunV args block env) -> return $ IntV 1
      Just _ -> throwException $ GenericRuntimeException pos

evalExpr (Neg pos expr1) = do
    val1 <- evalExpr expr1
    return $ IntV $ oneEvalInt negate val1

evalExpr (EAdd pos expr1 op expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case op of
      Plus ma -> return $ case val1 of
            (StrV _) -> StrV $ twoEvalString (++) val1 val2
            _ -> IntV $ twoEvalInt (+) val1 val2
      Minus ma -> return $ IntV $ twoEvalInt (-) val1 val2

evalExpr (EMul pos expr1 op expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case op of
      Times ma -> return $ IntV $ twoEvalInt (*) val1 val2
      Div ma -> if castInteger val2 == 0
          then throwException $ DivisionByZeroException pos
          else return $ IntV $ twoEvalInt div val1 val2
      Mod ma -> return $ IntV $ twoEvalInt mod val1 val2

evalExpr (Not pos expr1) = do
    val1 <- evalExpr expr1
    return $ BoolV $ oneEvalBool not val1

evalExpr (ERel pos expr1 op expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case op of
      LTH ma -> return $ BoolV $ val1 < val2
      LE ma -> return $ BoolV $  val1 <= val2
      GTH ma -> return $ BoolV $ val1 > val2
      GE ma -> return $ BoolV $  val1 >= val2
      EQU ma -> return $ BoolV $ val1 == val2
      NE ma -> return $ BoolV $  val1 /= val2

evalExpr (EAnd pos expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    return $ BoolV $ twoEvalBool (&&) val1 val2

evalExpr (EOr pos expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    return $ BoolV $ twoEvalBool (||) val1 val2

evalExpr (ELambda pos args rType block) = FunV args block <$> get
evalExpr (ELitInt _ val) = return $ IntV val
evalExpr (ELitChar _ val) = return $ CharV val
evalExpr (EString _ val) = return $ StrV val
evalExpr (ELitTrue _) = return $ BoolV True
evalExpr (ELitFalse _) = return $ BoolV False


----------EVAL BOOL--------------------------------
twoEvalBool :: (Bool -> Bool -> Bool) -> Value -> Value -> Bool
twoEvalBool f v1 v2 = f (castBool v1) (castBool v2)

oneEvalBool :: (Bool -> Bool) -> Value -> Bool
oneEvalBool f = f . castBool

---------EVAL INT---------------------------------
twoEvalInt :: (Integer -> Integer -> Integer) -> Value -> Value -> Integer
twoEvalInt f v1 v2 = f (castInteger v1) (castInteger v2)

oneEvalInt :: (Integer -> Integer) -> Value -> Integer
oneEvalInt f = f . castInteger

---------EVAL STR---------------------------------
twoEvalString :: (String -> String -> String) -> Value -> Value -> String
twoEvalString f v1 v2 = f (castString v1) (castString v2)

--------RND INIT----------------------------------
rndInit :: Type -> Value
rndInit (Int _) = IntV 0
rndInit (Str _) = StrV ""
rndInit (Char _) = CharV '\0'
rndInit (Bool _) = BoolV False
rndInit _ = undefined
module Evaluator.Evaluator where
import Evaluator.EvaluatorData
import Control.Monad.Trans.Except (ExceptT, throwE)
import Bnfc.AbsCringe
import Control.Monad.Trans.State (evalStateT, get, modify, put, gets)
import Control.Monad.IO.Class (MonadIO(liftIO))
import ProjectUtils (throwException, Raw (raw), rawVoid)
import Control.Monad (when, replicateM_, unless)
import Debug.Trace (trace)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Evaluator.EvaluatorUtils

eval :: Program -> ExceptT String IO String
eval (Program _ stmts) = do
    evalStateT (evalBlock stmts) initEvaluatorS
    --return "\n\n\x1b[32;1mCringe code execution successful ಠ_ಠ\x1b[0m"
    return ""
    
evalBlock :: [Stmt] -> EvaluatorState ()
evalBlock stmts = tryIgnoreEval $ do
    env <- get
    localEnv env (mapM_ evalStmt stmts)

--------------------------------STMT------------------------------------------------------------------------------
evalStmt :: Stmt -> EvaluatorState ()
evalStmt (Empty _) = return ()

evalStmt (BStmt _ (Block _ stmts)) = evalBlock stmts

evalStmt (Decl pos t (NoInit _ ident)) = tryIgnoreEval $ do
    st <- get
    put $ allocValue st ident (rndInit t)

evalStmt (Decl pos t (Init _ ident expr)) = tryIgnoreEval $ do
    val1 <- evalExpr expr
    st <- get
    put $ allocValue st ident val1

evalStmt (ConstDecl pos t item) = evalStmt(Decl pos t item)

evalStmt (Ass pos ident expr) = tryIgnoreEval $ do
    st <- get
    val1 <- evalExpr expr
    put $ setValue st ident val1

evalStmt (Incr pos ident) =
    evalStmt(Ass pos ident (EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)))

evalStmt (Decr pos ident) =
    evalStmt(Ass pos ident (EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)))

evalStmt (Ret pos expr) = tryIgnoreEval $ do
    st <- get
    val1 <- evalExpr expr
    put $ setReturnValue st (Just val1)

evalStmt (VRet pos) = tryIgnoreEval $ do
    st <- get
    put $ setReturnValue st (Just VoidV)

evalStmt (Cond pos expr stmt) = tryIgnoreEval $ do
    val1 <- evalExpr expr
    st <- get
    when (castBool val1) $ localEnv st (evalStmt stmt)

evalStmt (CondElse pos expr (Block _ ifBlock) (Block _ elseBlock)) = tryIgnoreEval $ do
    val1 <- evalExpr expr
    if castBool val1
        then evalBlock ifBlock
        else evalBlock elseBlock

evalStmt loop@(While pos expr stmt) = tryIgnoreEval $ do
    val1 <- evalExpr expr
    st <- get
    when (castBool val1) $ do
        localEnv st (evalStmt stmt)
        possibleFlag <- get
        put $ resetLoopFlags possibleFlag
        unless (wasBreak possibleFlag) $ evalStmt loop

evalStmt (For pos ident from to stmt) = tryIgnoreEval $ do
    val1 <- evalExpr from
    val2 <- evalExpr to
    st <- get
    localEnv (allocValue st ident (IntV $ castInteger val1 - 1)) (
        evalStmt (While pos (ERel pos (EVar pos ident) (LTH pos) (ELitInt pos (castInteger val2)))
        (BStmt pos (Block pos [Incr pos ident, stmt]))))


evalStmt (Print pos expr) = tryIgnoreEval $ do
    value <- evalExpr expr
    liftIO $ putStr $ show value

evalStmt (PrintLn pos expr) = tryIgnoreEval $ do
    value <- evalExpr expr
    liftIO $ print value

evalStmt (Break pos) = tryIgnoreEval $ do
    st <- get
    put $ setBreak st True

evalStmt (Continue pos) = tryIgnoreEval $ do
    st <- get
    put $ setContinue st True

evalStmt (SExp _ expr) = tryIgnoreEval $ do
  evalExpr expr
  return ()

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
      Just (FunV args rType (Block _ stmts) env) -> do
          vals <- mapM evalExpr exprs
          localEnv (functionEnv st env ident args exprs vals) (evalBlock stmts)
          newSt <- get
          put $ setReturnValue newSt Nothing
          case returnValue newSt of
            Nothing -> if rType == rawVoid
                            then return VoidV
                            else throwException $ NoReturnException pos ident
            Just ret -> return ret
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

evalExpr (ELambda pos args rType block) = FunV args (raw rType) block <$> gets env
evalExpr (ELitInt _ val) = return $ IntV val
evalExpr (ELitChar _ val) = return $ CharV val
evalExpr (EString _ val) = return $ StrV val
evalExpr (ELitTrue _) = return $ BoolV True
evalExpr (ELitFalse _) = return $ BoolV False


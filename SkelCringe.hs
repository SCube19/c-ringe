module SkelCringe where

-- Haskell module generated by the BNF converter

import AbsCringe
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program stmts -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ item -> failure x
  ConstDecl type_ item -> failure x
  Ass ident expr -> failure x
  Incr ident -> failure x
  Decr ident -> failure x
  Ret expr -> failure x
  VRet -> failure x
  Cond expr stmt -> failure x
  CondElse expr block1 block2 -> failure x
  While expr stmt -> failure x
  For ident expr1 expr2 stmt -> failure x
  Print expr -> failure x
  Break -> failure x
  Continue -> failure x
  SExp expr -> failure x
transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
transType :: Type -> Result
transType x = case x of
  Int -> failure x
  Char -> failure x
  Str -> failure x
  Bool -> failure x
  Void -> failure x
  Fun -> failure x
transArg :: Arg -> Result
transArg x = case x of
  Arg type_ ident -> failure x
  RefArg type_ ident -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar ident -> failure x
  ELitInt integer -> failure x
  ELitChar char -> failure x
  EString string -> failure x
  ELitTrue -> failure x
  ELitFalse -> failure x
  ELambda args type_ block -> failure x
  EApp ident exprs -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x

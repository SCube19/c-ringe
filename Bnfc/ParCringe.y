-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParCringe
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsCringe
import LexCringe

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('         { PT _ (TS _ 1)  }
  ')'         { PT _ (TS _ 2)  }
  '*'         { PT _ (TS _ 3)  }
  '+'         { PT _ (TS _ 4)  }
  ','         { PT _ (TS _ 5)  }
  '-'         { PT _ (TS _ 6)  }
  '->'        { PT _ (TS _ 7)  }
  '/'         { PT _ (TS _ 8)  }
  ';'         { PT _ (TS _ 9)  }
  '<'         { PT _ (TS _ 10) }
  '<='        { PT _ (TS _ 11) }
  '='         { PT _ (TS _ 12) }
  '>'         { PT _ (TS _ 13) }
  '>='        { PT _ (TS _ 14) }
  '['         { PT _ (TS _ 15) }
  ']'         { PT _ (TS _ 16) }
  'and'       { PT _ (TS _ 17) }
  'bool'      { PT _ (TS _ 18) }
  'char'      { PT _ (TS _ 19) }
  'dec'       { PT _ (TS _ 20) }
  'do'        { PT _ (TS _ 21) }
  'else'      { PT _ (TS _ 22) }
  'exit'      { PT _ (TS _ 23) }
  'false'     { PT _ (TS _ 24) }
  'from'      { PT _ (TS _ 25) }
  'fun'       { PT _ (TS _ 26) }
  'if'        { PT _ (TS _ 27) }
  'immutable' { PT _ (TS _ 28) }
  'inc'       { PT _ (TS _ 29) }
  'int'       { PT _ (TS _ 30) }
  'is'        { PT _ (TS _ 31) }
  'mod'       { PT _ (TS _ 32) }
  'not'       { PT _ (TS _ 33) }
  'or'        { PT _ (TS _ 34) }
  'print'     { PT _ (TS _ 35) }
  'printLn'   { PT _ (TS _ 36) }
  'ref'       { PT _ (TS _ 37) }
  'return'    { PT _ (TS _ 38) }
  'skip'      { PT _ (TS _ 39) }
  'string'    { PT _ (TS _ 40) }
  'to'        { PT _ (TS _ 41) }
  'true'      { PT _ (TS _ 42) }
  'void'      { PT _ (TS _ 43) }
  'while'     { PT _ (TS _ 44) }
  '{'         { PT _ (TS _ 45) }
  '}'         { PT _ (TS _ 46) }
  L_Ident     { PT _ (TV _)    }
  L_charac    { PT _ (TC _)    }
  L_integ     { PT _ (TI _)    }
  L_quoted    { PT _ (TL _)    }

%%

Ident :: { (AbsCringe.BNFC'Position, AbsCringe.Ident) }
Ident  : L_Ident { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Ident (tokenText $1)) }

Char    :: { (AbsCringe.BNFC'Position, Char) }
Char     : L_charac { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Char) }

Integer :: { (AbsCringe.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (AbsCringe.BNFC'Position, String) }
String   : L_quoted { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (AbsCringe.BNFC'Position, AbsCringe.Program) }
Program
  : ListStmt { (fst $1, AbsCringe.Program (fst $1) (snd $1)) }

Block :: { (AbsCringe.BNFC'Position, AbsCringe.Block) }
Block
  : '{' ListStmt '}' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Block (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (AbsCringe.BNFC'Position, [AbsCringe.Stmt]) }
ListStmt
  : {- empty -} { (AbsCringe.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (AbsCringe.BNFC'Position, AbsCringe.Stmt) }
Stmt
  : ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Empty (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, AbsCringe.BStmt (fst $1) (snd $1)) }
  | Type Item ';' { (fst $1, AbsCringe.Decl (fst $1) (snd $1) (snd $2)) }
  | 'immutable' Type Item ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.ConstDecl (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3)) }
  | Ident '=' Expr ';' { (fst $1, AbsCringe.Ass (fst $1) (snd $1) (snd $3)) }
  | 'inc' Ident ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Incr (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'dec' Ident ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Decr (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' Expr ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Ret (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.VRet (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'if' '(' Expr ')' Stmt { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Cond (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Block 'else' Block { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.CondElse (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Stmt { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.While (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'from' Ident '=' Expr 'to' Expr 'do' Stmt { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.For (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $6) (snd $8)) }
  | 'print' Expr ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Print (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'printLn' Expr ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.PrintLn (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'exit' ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Break (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'skip' ';' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Continue (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | Expr ';' { (fst $1, AbsCringe.SExp (fst $1) (snd $1)) }

Item :: { (AbsCringe.BNFC'Position, AbsCringe.Item) }
Item
  : Ident { (fst $1, AbsCringe.NoInit (fst $1) (snd $1)) }
  | Ident '=' Expr { (fst $1, AbsCringe.Init (fst $1) (snd $1) (snd $3)) }

Type :: { (AbsCringe.BNFC'Position, AbsCringe.Type) }
Type
  : 'int' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Int (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'char' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Char (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Str (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'bool' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Bool (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'void' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Void (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'fun' '[' ListArgType '->' Type ']' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Fun (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }

ArgType :: { (AbsCringe.BNFC'Position, AbsCringe.ArgType) }
ArgType
  : Type { (fst $1, AbsCringe.Val (fst $1) (snd $1)) }
  | 'ref' Type { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Ref (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListArgType :: { (AbsCringe.BNFC'Position, [AbsCringe.ArgType]) }
ListArgType
  : {- empty -} { (AbsCringe.BNFC'NoPosition, []) }
  | ArgType { (fst $1, (:[]) (snd $1)) }
  | ArgType ',' ListArgType { (fst $1, (:) (snd $1) (snd $3)) }

Arg :: { (AbsCringe.BNFC'Position, AbsCringe.Arg) }
Arg
  : ArgType Ident { (fst $1, AbsCringe.Arg (fst $1) (snd $1) (snd $2)) }

ListArg :: { (AbsCringe.BNFC'Position, [AbsCringe.Arg]) }
ListArg
  : {- empty -} { (AbsCringe.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr6
  : Ident { (fst $1, AbsCringe.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, AbsCringe.ELitInt (fst $1) (snd $1)) }
  | Char { (fst $1, AbsCringe.ELitChar (fst $1) (snd $1)) }
  | String { (fst $1, AbsCringe.EString (fst $1) (snd $1)) }
  | 'true' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.ELitTrue (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.ELitFalse (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | '(' ListArg ')' '->' Type Block { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.ELambda (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5) (snd $6)) }
  | Ident '(' ListExpr ')' { (fst $1, AbsCringe.EApp (fst $1) (snd $1) (snd $3)) }
  | '(' Expr ')' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr5
  : '-' Expr6 { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Neg (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'not' Expr6 { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Not (uncurry AbsCringe.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, AbsCringe.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, AbsCringe.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, AbsCringe.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr1
  : Expr2 'and' Expr1 { (fst $1, AbsCringe.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (AbsCringe.BNFC'Position, AbsCringe.Expr) }
Expr
  : Expr1 'or' Expr { (fst $1, AbsCringe.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (AbsCringe.BNFC'Position, [AbsCringe.Expr]) }
ListExpr
  : {- empty -} { (AbsCringe.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (AbsCringe.BNFC'Position, AbsCringe.AddOp) }
AddOp
  : '+' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Plus (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Minus (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (AbsCringe.BNFC'Position, AbsCringe.MulOp) }
MulOp
  : '*' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Times (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Div (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'mod' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.Mod (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (AbsCringe.BNFC'Position, AbsCringe.RelOp) }
RelOp
  : '<' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.LTH (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.LE (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.GTH (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.GE (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'is' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.EQU (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }
  | 'is' 'not' { (uncurry AbsCringe.BNFC'Position (tokenLineCol $1), AbsCringe.NE (uncurry AbsCringe.BNFC'Position (tokenLineCol $1))) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err AbsCringe.Program
pProgram = fmap snd . pProgram_internal
}


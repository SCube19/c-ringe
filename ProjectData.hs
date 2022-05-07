module ProjectData where
import  AbsCringe
import ProjectUtils
import Data.Map (Map)
import Prelude 


-----------TYPE CHECKER ENVIRONMENT----------------------------------------------------------------------
data TypeCheckerEnv = TypeCheckerEnv {
  types :: Map Ident Type,
  expectedReturnType :: Maybe RetType,
  insideLoop :: Bool
} 



--------------EXCEPTIONS---------------------------------------------------------------------------------
data TypeCheckerException  =  InvalidTypeException BNFC'Position Type Type
                            | InvalidFunctionApplication BNFC'Position Ident Type
                            | InvalidReturnTypeException BNFC'Position Type Type
                            | InvalidReturnException BNFC'Position
                            | InvalidExitException BNFC'Position
                            | InvalidSkipException BNFC'Position
                            | MustBeVariableException BNFC'Position
                            | UndefinedException BNFC'Position Ident
                            | WildCardException BNFC'Position

data InterpreterException =   NoReturnException BNFC'Position Ident
                            | DivisionByZeroException BNFC'Position Ident

instance Show TypeCheckerException where

  show (InvalidTypeException position type1 type2) = 
    "Static Error: Invalid TYPE of " ++ prettyType type1 ++ " at " ++ prettyPosition position ++ "; EXPECTED " ++ prettyType type2 

  show (InvalidFunctionApplication position ident t) = 
    "Static Error: Invalid FUNCTION APPLICATION at " ++ prettyPosition position ++ "; '" ++ prettyIdent ident ++ "' is of type " ++ prettyType t

  show (InvalidReturnTypeException position type1 type2) = 
    "Static Error: Invalid RETURN statement's TYPE of " ++ prettyType type1 ++ " at " ++ prettyPosition position ++ "; EXPECTED " ++ prettyType type2

  show (InvalidReturnException position) = 
    "Static Error: RETURN statement OUTSIDE of a function definition block at " ++ prettyPosition position

  show (InvalidExitException position) = 
    "Static Error: EXIT statement OUTSIDE of a loop block at " ++ prettyPosition position

  show (InvalidSkipException position) = 
    "Static Error: SKIP statement OUTSIDE of a loop block at " ++ prettyPosition position

  show (MustBeVariableException position) = 
    "Static Error: Expression at " ++ prettyPosition position ++ " must be a variable"

  show (UndefinedException position ident) = 
    "Static Error: UNDEFINED IDENTIFIER '" ++ prettyIdent ident ++ "' at " ++ prettyPosition position
  
  show (WildCardException position) =
    "Static Error: Unknown problem at " ++ prettyPosition position


instance Show InterpreterException where
  show (NoReturnException position ident) = 
    "Runtime Error: After execution of '" ++ prettyIdent  ident ++ "' NO RETURN statement was encountered at " ++ prettyPosition position

  show (DivisionByZeroException position ident) =
    "Runtime Error: Identifier '" ++ prettyIdent ident ++ "' IS 0 at " ++ prettyPosition position 


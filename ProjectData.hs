{-# LANGUAGE DeriveTraversable #-}

module ProjectData where
import AbsCringe
import ProjectUtils
import qualified Data.Map as M 
import Control.Monad.Trans.Except (ExceptT, Except)
import Control.Monad.Trans.State (StateT, get, modify, put)
import Control.Monad.Trans.Reader (ReaderT)

-----------TYPE CHECKER ENVIRONMENT---------------------------------------------------------------------
type TypeCheckerState = StateT TypeCheckerEnv (ExceptT String IO)

localState :: Monad m => r -> StateT r m a -> StateT r m ()
localState changed action = do
  backup <- get
  put changed
  action
  put backup

data TypeCheckerEnv = TypeCheckerEnv {
  types :: M.Map Ident (Type, Bool),
  expectedReturnType :: Maybe Type,
  insideLoop :: Bool
} deriving (Show)

initTypeCheckerEnv :: TypeCheckerEnv
initTypeCheckerEnv = TypeCheckerEnv {
  types = M.empty,
  expectedReturnType = Nothing,
  insideLoop = False
}

getType :: TypeCheckerEnv -> Ident -> Maybe (Type, Bool)
getType env i = M.lookup i (types env)

setTypes :: TypeCheckerEnv -> [Ident] -> [(Type, Bool)] -> TypeCheckerEnv
setTypes env _ [] = env  
setTypes env [] _ = env
setTypes env (i:is) (t:ts) = setTypes (setType env i t) is ts

setType :: TypeCheckerEnv -> Ident -> (Type, Bool) -> TypeCheckerEnv
setType env i t = TypeCheckerEnv {
  types = M.insert i t (types env),
  expectedReturnType = expectedReturnType env,
  insideLoop = insideLoop env
}

setExpectedReturnType :: TypeCheckerEnv -> Maybe Type -> TypeCheckerEnv
setExpectedReturnType env r = TypeCheckerEnv {
  types = types env,
  expectedReturnType = r,
  insideLoop = insideLoop env
}

setInsideLoop :: TypeCheckerEnv -> Bool -> TypeCheckerEnv
setInsideLoop env b = TypeCheckerEnv {
  types = types env,
  expectedReturnType = expectedReturnType env,
  insideLoop = b
}

--------------EXCEPTIONS---------------------------------------------------------------------------------
data TypeCheckerException  =  InvalidTypeExpectedException BNFC'Position Type Type
                            | InvalidTypeException BNFC'Position Type 
                            | InvalidFunctionApplicationException BNFC'Position Ident Type
                            | InvalidNumberOfParametersException BNFC'Position
                            | InvalidReturnException BNFC'Position
                            | InvalidExitException BNFC'Position
                            | InvalidSkipException BNFC'Position
                            | UndefinedException BNFC'Position Ident
                            | VoidNotAllowedException BNFC'Position
                            | RedeclarationException BNFC'Position Ident
                            | ConstReferenceException BNFC'Position
                            | ReferenceNotVariableException BNFC'Position
                            | ImmutableNotInitializedException BNFC'Position Ident
                            | ImmutableCannotBeChangedException BNFC'Position Ident
                            | WildCardException BNFC'Position

data InterpreterException =   NoReturnException BNFC'Position Ident
                            | DivisionByZeroException BNFC'Position Ident

instance Show TypeCheckerException where

  show (InvalidTypeExpectedException position type1 type2) =
    "Static Error: Invalid TYPE of " ++ prettyType type1 ++ " at " ++ prettyPosition position ++ "; EXPECTED " ++ prettyType type2

  show (InvalidTypeException position type1) =
    "Static Error: Invalid TYPE of " ++ prettyType type1 ++ " at " ++ prettyPosition position

  show (InvalidFunctionApplicationException position ident t) =
    "Static Error: Invalid FUNCTION APPLICATION at " ++ prettyPosition position ++ "; '" ++ prettyIdent ident ++ "' is of type " ++ prettyType t

  show (InvalidNumberOfParametersException position) = 
    "Static Error: Invalid NUMBER OF PARAMETERS at " ++ prettyPosition position

  show (InvalidReturnException position) =
    "Static Error: RETURN statement OUTSIDE of a function definition block at " ++ prettyPosition position

  show (InvalidExitException position) =
    "Static Error: EXIT statement OUTSIDE of a loop block at " ++ prettyPosition position

  show (InvalidSkipException position) =
    "Static Error: SKIP statement OUTSIDE of a loop block at " ++ prettyPosition position

  show (UndefinedException position ident) =
    "Static Error: UNDEFINED IDENTIFIER '" ++ prettyIdent ident ++ "' at " ++ prettyPosition position

  show (VoidNotAllowedException position) = 
    "Static Error: VOID type NOT ALLOWED outside function return type at " ++ prettyPosition position

  show (RedeclarationException position ident) = 
    "Static Error: REDECLARATION of variable '" ++ prettyIdent ident ++ "' at " ++ prettyPosition position

  show (ConstReferenceException position) =
    "Static Error: cannot pass IMMUTABLE variable as REFERENCE parameter at " ++ prettyPosition position

  show (ReferenceNotVariableException position) =
    "Static Error: REFERENCE parameter MUST BE VARIABLE at " ++ prettyPosition position

  show (ImmutableNotInitializedException position ident) = 
    "Static Error: IMMUTABLE value '" ++ prettyIdent ident ++ "' MUST BE INITIALIZED at " ++ prettyPosition position

  show (ImmutableCannotBeChangedException position ident) = 
    "Static Error: IMMUTABLE value '" ++ prettyIdent ident ++ "' CANNOT BE ASSIGNED new value at " ++ prettyPosition position

  show (WildCardException position) =
    "Static Error: Unknown problem at " ++ prettyPosition position


instance Show InterpreterException where
  show (NoReturnException position ident) =
    "Runtime Error: After execution of '" ++ prettyIdent  ident ++ "' NO RETURN statement was encountered at " ++ prettyPosition position

  show (DivisionByZeroException position ident) =
    "Runtime Error: Identifier '" ++ prettyIdent ident ++ "' IS 0 at " ++ prettyPosition position


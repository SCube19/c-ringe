module ProjectData where
import AbsCringe
import ProjectUtils
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Except (ExceptT, Except)
import Control.Monad.Trans.State (StateT, get, modify, put, gets)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import InteractiveEval (Term(val))

-----------EVALUATOR ENVIRONMENT------------------------------------------------------------------------
type EvaluatorState = StateT EvaluatorS (ExceptT String IO)

data Value =  IntV Integer
            | BoolV Bool
            | StrV String
            | CharV Char
            | FunV [Arg] Block EvaluatorS
            | VoidV

instance Show Value where
  show (IntV v) = show v
  show (BoolV v) = map toLower $ show v
  show (StrV v) = v
  show (CharV v) = [v]
  show f@(FunV args block env) = "???FUNCTION???"
  show VoidV = ""

instance Eq Value where
  (==) (IntV v1) (IntV v2) = v1 == v2
  (==) (BoolV v1) (BoolV v2) = v1 == v2
  (==) (StrV v1) (StrV v2) = v1 == v2
  (==) (CharV v1) (CharV v2) = v1 == v2
  (==) _ _ = undefined

instance Ord Value where
  (<=) (IntV v1) (IntV v2) = v1 <= v2
  (<=) (BoolV v1) (BoolV v2) = v1 <= v2
  (<=) (StrV v1) (StrV v2) = v1 <= v2
  (<=) (CharV v1) (CharV v2) = v1 <= v2
  (<=) _ _ = undefined

castBool :: Value -> Bool
castBool (IntV v) = v > 0
castBool (BoolV v) = v
castBool _ = False

castInteger :: Value -> Integer
castInteger (IntV v) = v
castInteger (BoolV v) = if v then 1 else 0
castInteger _ = 0

castString :: Value -> String
castString (StrV v) = v
castString _ = ""

type Loc = Integer

data EvaluatorS = EvaluatorS {
  env :: M.Map Ident Loc,
  store :: M.Map Loc Value,
  newloc :: Loc
}

initEvaluatorS :: EvaluatorS
initEvaluatorS = EvaluatorS {
  env = M.empty,
  store = M.empty,
  newloc = 0
}

getValue :: EvaluatorS -> Ident -> Maybe Value
getValue s ident = M.lookup (fromMaybe (-1) (M.lookup ident (env s))) (store s)

allocValues :: EvaluatorS -> [Ident] -> [Value] -> EvaluatorS
allocValues s _ [] = s
allocValues s [] _ = s
allocValues s (i:is) (v:vs) = allocValues (allocValue s i v) is vs

allocValue :: EvaluatorS -> Ident -> Value -> EvaluatorS
allocValue s ident value = EvaluatorS {
  env = M.insert ident (newloc s) (env s),
  store = M.insert (newloc s) value (store s),
  newloc = newloc s + 1
}

setValues :: EvaluatorS -> [Ident] -> [Value] -> EvaluatorS
setValues s _ [] = s
setValues s [] _ = s
setValues s (i:is) (v:vs) = setValues (setValue s i v) is vs

setValue :: EvaluatorS -> Ident -> Value -> EvaluatorS
setValue s ident value = 
  case M.lookup ident (env s) of 
    Nothing -> allocValue s ident value 
    Just loc -> EvaluatorS {
      env = env s,
      store = M.insert loc value (store s),
      newloc = newloc s
    }

-- localEnv :: TypeCheckerS -> TypeCheckerState () -> TypeCheckerState ()
-- localEnv changedEnv action = do
--   originalS <- get 
--   put changedEnv
--   action
--   changedStoreS <- get 
--   put $ setTypeStore originalS (typeStore changedStoreS)

-----------TYPE CHECKER ENVIRONMENT---------------------------------------------------------------------
type TypeCheckerState = StateT TypeCheckerS (ExceptT String IO)

data TypeCheckerS = TypeCheckerS {
  typeEnv :: M.Map Ident (Type, Bool),
  scope :: S.Set Ident,
  expectedReturnType :: Maybe Type,
  insideLoop :: Bool
} deriving (Show)

initTypeCheckerS :: TypeCheckerS
initTypeCheckerS = TypeCheckerS {
  typeEnv = M.empty,
  scope = S.empty,
  expectedReturnType = Nothing,
  insideLoop = False
}

getType :: TypeCheckerS -> Ident -> Maybe (Type, Bool)
getType s ident = M.lookup ident (typeEnv s)

setTypes :: TypeCheckerS -> [Ident] -> [(Type, Bool)] -> TypeCheckerS
setTypes s _ [] = s
setTypes s [] _ = s
setTypes s (i:is) (t:ts) = setTypes (setType s i t) is ts

setType :: TypeCheckerS -> Ident -> (Type, Bool) -> TypeCheckerS
setType s ident t = TypeCheckerS {
      typeEnv = M.insert ident t (typeEnv s),
      scope = S.insert ident (scope s),
      expectedReturnType = expectedReturnType s,
      insideLoop = insideLoop s
    }

setExpectedReturnType :: TypeCheckerS -> Maybe Type -> TypeCheckerS
setExpectedReturnType s r = TypeCheckerS {
  typeEnv = typeEnv s,
  scope = scope s,
  expectedReturnType = r,
  insideLoop = insideLoop s
}

setInsideLoop :: TypeCheckerS -> Bool -> TypeCheckerS
setInsideLoop s b = TypeCheckerS {
  typeEnv = typeEnv s,
  scope = scope s,
  expectedReturnType = expectedReturnType s,
  insideLoop = b
}

emptyScope :: TypeCheckerS -> TypeCheckerS
emptyScope s = TypeCheckerS {
  typeEnv = typeEnv s,
  scope = S.empty,
  expectedReturnType = expectedReturnType s,
  insideLoop = insideLoop s
} 

localTypeEnv :: TypeCheckerS -> TypeCheckerState () -> TypeCheckerState ()
localTypeEnv changedEnv action = do
  backup <- get 
  put changedEnv
  action
  put backup

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
                            | DivisionByZeroException BNFC'Position
                            | GenericRuntimeException BNFC'Position

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

  show (DivisionByZeroException position) =
    "Runtime Error: Expression IS 0 at " ++ prettyPosition position

  show (GenericRuntimeException position) =
    "Runtime Error: Unknown problem at " ++ prettyPosition position
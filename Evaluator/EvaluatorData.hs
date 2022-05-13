module Evaluator.EvaluatorData where
import Control.Monad.Trans.State (StateT, get, put)
import qualified Data.Map as M
import Bnfc.AbsCringe (Ident, Arg, Type, Block, BNFC'Position)
import Control.Monad.Trans.Except (ExceptT)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import ProjectUtils (prettyIdent, prettyPosition)

type EvaluatorState = StateT EvaluatorS (ExceptT String IO)
type Environment = M.Map Ident Loc
type Store = M.Map Loc Value 

data Value =  IntV Integer
            | BoolV Bool
            | StrV String
            | CharV Char
            | FunV [Arg] Type Block Environment
            | VoidV

instance Show Value where
  show (IntV v) = show v
  show (BoolV v) = map toLower $ show v
  show (StrV v) = v
  show (CharV v) = [v]
  show f@(FunV args rType block env) = "???FUNCTION???"
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
  env :: Environment,
  store :: Store,
  newloc :: Loc,
  wasBreak :: Bool,
  wasContinue :: Bool,
  returnValue :: Maybe Value
} deriving Show

initEvaluatorS :: EvaluatorS
initEvaluatorS = EvaluatorS {
  env = M.empty,
  store = M.empty,
  newloc = 0,
  wasBreak = False,
  wasContinue = False,
  returnValue = Nothing
} 

getLoc :: EvaluatorS -> Ident -> Maybe Loc
getLoc s ident = M.lookup ident (env s)

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
  newloc = newloc s + 1,
  wasBreak = wasBreak s,
  wasContinue = wasContinue s,
  returnValue = returnValue s
}

setLoc :: EvaluatorS -> Ident -> Loc -> EvaluatorS
setLoc s ident loc = EvaluatorS {
  env = M.insert ident loc (env s),
  store = store s,
  newloc = newloc s + 1,
  wasBreak = wasBreak s,
  wasContinue = wasContinue s,
  returnValue = returnValue s
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
      newloc = newloc s,
      wasBreak = wasBreak s,
      wasContinue = wasContinue s,
      returnValue = returnValue s
    }

setEnv :: EvaluatorS -> Environment -> EvaluatorS
setEnv s e = EvaluatorS {
  env = e,
  store = store s,
  newloc = newloc s,
  wasBreak = wasBreak s,
  wasContinue = wasContinue s,
  returnValue = returnValue s
}

setBreak :: EvaluatorS -> Bool -> EvaluatorS
setBreak s b = EvaluatorS {
  env = env s,
  store = store s,
  newloc = newloc s,
  wasBreak = b,
  wasContinue = wasContinue s,
  returnValue = returnValue s
}

setContinue :: EvaluatorS -> Bool -> EvaluatorS
setContinue s b = EvaluatorS {
  env = env s,
  store = store s,
  newloc = newloc s,
  wasBreak = wasBreak s,
  wasContinue = b,
  returnValue = returnValue s
}

setReturnValue :: EvaluatorS -> Maybe Value -> EvaluatorS
setReturnValue s v = EvaluatorS {
  env = env s,
  store = store s,
  newloc = newloc s,
  wasBreak = wasBreak s,
  wasContinue = wasContinue s,
  returnValue = v
}

resetLoopFlags :: EvaluatorS -> EvaluatorS
resetLoopFlags s = EvaluatorS {
  env = env s,
  store = store s,
  newloc = newloc s,
  wasBreak = False,
  wasContinue = False,
  returnValue = returnValue s
}

localEnv :: EvaluatorS -> EvaluatorState () -> EvaluatorState ()
localEnv changedEnv action = do
  originalS <- get 
  put changedEnv
  action
  changedStoreS <- get 
  put $ setEnv changedStoreS (env originalS)


---------------------EXCEPTIONS------------------------------------------
data InterpreterException =   NoReturnException BNFC'Position Ident
                            | DivisionByZeroException BNFC'Position
                            | GenericRuntimeException BNFC'Position


instance Show InterpreterException where
  show (NoReturnException position ident) =
    "Runtime Error: After execution of '" ++ prettyIdent ident ++ "' NO RETURN statement was encountered at " ++ prettyPosition position

  show (DivisionByZeroException position) =
    "Runtime Error: Expression IS 0 at " ++ prettyPosition position

  show (GenericRuntimeException position) =
    "Runtime Error: Unknown problem at " ++ prettyPosition position
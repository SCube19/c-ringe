module Evaluator.EvaluatorUtils where
import Evaluator.EvaluatorData 
import Bnfc.AbsCringe 
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (unless)
import Control.Monad.Trans.State

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
rndInit (Int _) = IntV  0
rndInit (Str _) = StrV  ""
rndInit (Char _) = CharV  '\0'
rndInit (Bool _) = BoolV False
rndInit _ = undefined

----------IGNORE--------------------------------------
ignore :: EvaluatorS -> Bool
ignore s = wasBreak s || wasContinue s || isJust (returnValue s)

tryIgnoreEval :: EvaluatorState () -> EvaluatorState ()
tryIgnoreEval action = do
    s <- get
    unless (ignore s) action

------------FUNCTION ENV-----------------------------------
functionEnv :: EvaluatorS -> Environment -> Ident -> [Arg] -> [Expr] -> [Value] -> EvaluatorS
functionEnv s funEnv f = funcArgs (allocValue (setEnv s funEnv) f (fromMaybe undefined (getValue s f))) s

funcArgs :: EvaluatorS -> EvaluatorS -> [Arg] -> [Expr] -> [Value] -> EvaluatorS
funcArgs s _ [] [] [] = s
funcArgs s _ [] _ _ = s
funcArgs s _ _ [] _ = s
funcArgs s _ _ _ [] = s
funcArgs s recentS ((Arg _ (Val _ _) ident):args) (expr:exprs) (val:vals) =
    funcArgs (allocValue s ident val) recentS args exprs vals

funcArgs s recentS a@((Arg _ (Ref _ _ ) ident):args) e@((EVar _ varIdent):exprs) v@(val:vals) =
    funcArgs (setLoc s ident (fromMaybe undefined $ getLoc recentS varIdent)) recentS args exprs vals

funcArgs s _ ((Arg _ (Ref _ _) ident):_) _ _  = undefined
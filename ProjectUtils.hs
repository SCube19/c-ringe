{-# LANGUAGE FlexibleInstances #-}
module ProjectUtils where
import Bnfc.AbsCringe

import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO ( stderr, hPutStrLn )
import Data.List ( intercalate )
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Set as Set
import Control.Monad.Trans.State (StateT, get, put)
------------------------------GENERAL--------------------------------------------------------
exitError :: String -> IO ()
exitError e = do
    hPutStrLn stderr ("FATAL: " ++ e)
    exitFailure

class Typical a where
    toType :: a -> Type

instance Typical ArgType where
    toType (Val _ t) = t
    toType (Ref _ t) = t

getArgIdent :: Arg -> Ident
getArgIdent (Arg _ t ident) = ident

getArgType :: Arg -> ArgType
getArgType (Arg _ t _) = t

makeFalsyTuple :: a -> (a, Bool)
makeFalsyTuple x = (x, False)

throwException x = lift $ throwE $ show x

firstDuplicateIndex :: Ord a => [a] -> Maybe Int
firstDuplicateIndex xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s
                           then Just $ length s
                           else dup' xs (Set.insert x s)

-------------PRETTY-------------------------------------------------------------------

prettyPosition :: BNFC'Position -> String
prettyPosition (Just (row, col)) = "position " ++ show row ++ ":" ++ show col
prettyPosition _ = "???"

prettyIdent :: Ident -> String
prettyIdent (Ident x) = x

prettyType :: Type -> String
prettyType (Int _) = "int"
prettyType (Char _) = "char"
prettyType (Str _) = "string"
prettyType (Bool _) = "bool"
prettyType (Void _) = "void"
prettyType (Fun _ args rType) = "fun [" ++ intercalate "," (map prettyArgType args) ++ " -> " ++ prettyType rType ++ "]"

prettyArgType :: ArgType -> String
prettyArgType (Val _ t) = prettyType t
prettyArgType (Ref _ t) = "ref " ++ prettyType t

------------------------------------TYPE MANIP--------------------------------------------
class Raw a where
    raw :: a -> a
    cleanRaw :: a -> a
    cleanRaw = raw

instance Raw Type where
    raw (Int _) = rawInt
    raw (Char _) = rawChar
    raw (Str _) = rawStr
    raw (Bool _) = rawBool
    raw (Fun _ args rType) = Fun Nothing (map raw args) (raw rType)
    raw (Void _) = Void Nothing
    cleanRaw Fun {} = rawFun
    cleanRaw x = raw x

instance Raw ArgType where
    raw (Val _ t) = Val Nothing (raw t)
    raw (Ref _ t) = Ref Nothing (raw t)

rawInt = Int Nothing
rawChar = Char Nothing
rawStr = Str Nothing
rawBool = Bool Nothing
rawFun = Fun Nothing [] (Void Nothing)
rawVoid = Void Nothing

{-# LANGUAGE FlexibleInstances #-}
module ProjectUtils where
import AbsCringe

import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO
import Data.List

------------------------------GENERAL--------------------------------------------------------
exitError :: String -> IO ()
exitError e = do
    hPutStrLn stderr e
    exitFailure

class Positional a where
    getPos :: a -> BNFC'Position

instance Positional Type where
    getPos (Int pos) = pos
    getPos (Char pos) = pos
    getPos (Str pos) = pos
    getPos (Bool pos) = pos
    getPos (Fun pos _ _) = pos

getArgType :: Arg -> ArgType
getArgType (Arg _ t _) = t
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
prettyType (Fun _ args rType) = "fun [" ++ intercalate "," (map prettyArgType args) ++ " -> " ++ prettyRetType rType ++ "]"

prettyArgType :: ArgType -> String
prettyArgType (Val _ t) = prettyType t
prettyArgType (Ref _ t) = "ref " ++ prettyType t

prettyRetType :: RetType -> String
prettyRetType (Void _) = ""
prettyRetType (NoVoid _ t) = prettyType t

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
    cleanRaw Fun {} = rawFun
    cleanRaw x = raw x

instance Raw RetType where
    raw (NoVoid _ t) = NoVoid Nothing (raw t)
    raw (Void _) = Void Nothing

instance Raw ArgType where
    raw (Val _ t) = Val Nothing (raw t)
    raw (Ref _ t) = Ref Nothing (raw t)

rawInt = Int Nothing
rawChar = Char Nothing
rawStr = Str Nothing
rawBool = Bool Nothing
rawFun = Fun Nothing [] (Void Nothing)
rawVoid = Void Nothing

--------------------TYPE CHECKER UTILS----------------------------------------------------


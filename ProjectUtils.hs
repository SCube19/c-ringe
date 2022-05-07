module ProjectUtils where
import AbsCringe

import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO
import Data.List 

exitError :: String -> IO ()
exitError e = do
    hPutStrLn stderr e
    exitFailure

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
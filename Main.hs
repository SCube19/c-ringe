import AbsCringe 
import TypeChecker
import ProjectUtils
import ProjectData 
import Interpreter

import ParCringe (myLexer, pProgram)
import System.Directory.Internal.Prelude (exitFailure, getArgs)
import System.Exit (exitSuccess)
import System.IO
import Control.Monad.Trans.Except
import LexCringe (tokens)

tokenize :: String -> ExceptT String IO Program
tokenize s = case pProgram $ myLexer s of
  Left str -> throwE str
  Right prog -> return prog

runProgram :: String -> ExceptT String IO Int
runProgram s = do
  tokens <- tokenize s
  typecheck <- typeCheck tokens
  interpret tokens

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      contents <- getContents
      result <- runExceptT $ runProgram contents
      either exitError print result
    [file] -> do
      program <- readFile file
      result <- runExceptT $ runProgram program
      either exitError print result

    _ -> exitFailure

test :: String -> IO ()
test s = do
  result <- runExceptT $ runProgram s
  either exitError print result
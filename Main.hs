import Bnfc.AbsCringe (Program)
import Bnfc.LexCringe (tokens)
import Bnfc.ParCringe (myLexer, pProgram)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Evaluator.Evaluator (eval)
import ProjectUtils (exitError)
import System.Directory.Internal.Prelude (exitFailure, getArgs)
import System.Exit (exitSuccess)
import System.IO ()
import Typechecker.TypeChecker (typeCheck)

tokenize :: String -> ExceptT String IO Program
tokenize s = case pProgram $ myLexer s of
  Left str -> throwE str
  Right prog -> return prog

runProgram :: String -> ExceptT String IO String
runProgram s = do
  tokens <- tokenize s
  typecheck <- typeCheck tokens
  eval tokens

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      contents <- getContents
      result <- runExceptT $ runProgram contents
      either exitError putStrLn result
    [file] -> do
      program <- readFile file
      result <- runExceptT $ runProgram program
      either exitError putStrLn result
    _ -> exitFailure

test :: String -> IO ()
test s = do
  result <- runExceptT $ runProgram s
  either exitError print result
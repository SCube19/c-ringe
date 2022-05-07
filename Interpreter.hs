module Interpreter where 
import ProjectData
import Control.Monad.Trans.Except (ExceptT, throwE)
import AbsCringe

interpret :: Program -> ExceptT String IO Int
interpret program = throwE $ show $ DivisionByZeroException (Just (1, 2)) (Ident "x")
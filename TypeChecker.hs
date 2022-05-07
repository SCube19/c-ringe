module TypeChecker where
import AbsCringe
import ProjectData 
import Control.Monad.Trans.Except

typeCheck :: Program -> ExceptT String IO ()
typeCheck program = 
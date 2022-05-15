all : test

test : interpreter
	chmod +x test.sh && ./test.sh

interpreter : Main.hs ProjectUtils.hs evaluator bnfc typechecker
	ghc Main.hs -o interpreter

bnfc : Bnfc/AbsCringe.hs Bnfc/LexCringe.hs Bnfc/ParCringe.hs

evaluator : Evaluator/Evaluator.hs Evaluator/EvaluatorData.hs Evaluator/EvaluatorUtils.hs

typechecker : Typechecker/TypeChecker.hs Typechecker/TypeCheckerData.hs 

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi interpreter
	-rm -f Evaluator/*.o Evaluator/*.hi
	-rm -f Typechecker/*.o Typechecker/*.hi
	-make -C Bnfc clean
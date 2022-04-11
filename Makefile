all:
	happy -gca ParCringe.y
	alex -g LexCringe.x
	ghc --make TestCringe.hs -o TestCringe

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCringe.* LexCringe.* ParCringe.* LayoutCringe.* SkelCringe.* PrintCringe.* TestCringe.* AbsCringe.* TestCringe ErrM.* SharedString.* ComposOp.* cringe.dtd XMLCringe.* Makefile*
	


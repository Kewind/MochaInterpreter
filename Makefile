all:
	happy -gca ParMocha.y
	alex -g LexMocha.x
	ghc --make Main.hs -o Main

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMocha.* LexMocha.* ParMocha.* LayoutMocha.* PrintMocha.* AbsMocha.* ErrM.* SharedString.* ComposOp.* Mocha.dtd XMLMocha.* Makefile*

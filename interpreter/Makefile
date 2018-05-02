all:
	happy -gca ParGrammar.y
	alex -g LexGrammar.x
	ghc --make Main.hs -o Main

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi


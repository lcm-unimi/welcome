all: welcome

welcome: welcome.hs deps
	ghc $< -o welcometolcm

deps:
	cabal update && cabal install hscurses
	touch deps

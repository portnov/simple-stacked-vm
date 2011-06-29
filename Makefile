GHC=ghc --make -fwarn-unused-imports -fwarn-incomplete-patterns

all: ssvm

ssvm: ssvm.hs Language/SSVM/*.hs
	$(GHC) $<

install:
	cabal install --global

clean:
	rm -f ssvm
	find . -name \*.hi -delete
	find . -name \*.o -delete

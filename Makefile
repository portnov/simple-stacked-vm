GHC=ghc --make -fwarn-unused-imports

all: ssvm

ssvm: ssvm.hs Language/SSVM/*.hs
	$(GHC) $<

clean:
	rm ssvm
	find . -name *.hi -delete
	find . -name *.o -delete

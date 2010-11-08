.PHONY: clean ints exps exps1 exps2

GHC_OPTS=-O2 -funbox-strict-fields -fwarn-unused-imports
PROGS=ints exps exps1 exps2

all: $(PROGS)

ints:
	ghc --make $(GHC_OPTS) ints.hs	

exps:
	ghc --make $(GHC_OPTS) exps.hs	

exps1:
	ghc --make $(GHC_OPTS) exps1.hs	

exps2:
	ghc --make $(GHC_OPTS) exps2.hs	

clean:
	rm -f $(PROGS) *.o *.hi *~



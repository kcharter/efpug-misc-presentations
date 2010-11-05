.PHONY: clean ints exps exps1

GHC_OPTS=-O2 -funbox-strict-fields
PROGS=ints exps exps1

all: $(PROGS)

ints:
	ghc --make $(GHC_OPTS) ints.hs	

exps:
	ghc --make $(GHC_OPTS) exps.hs	

exps1:
	ghc --make $(GHC_OPTS) exps1.hs	

clean:
	rm -f $(PROGS) *.o *.hi *~



.PHONY: clean ints exps

GHC_OPTS=-O2 -funbox-strict-fields
PROGS=ints exps

all: $(PROGS)

ints:
	ghc --make $(GHC_OPTS) ints.hs	

exps:
	ghc --make $(GHC_OPTS) exps.hs	

clean:
	rm -f $(PROGS) *.o *.hi *~



.PHONY: clean ints

GHC_OPTS=-O2 -funbox-strict-fields
PROGS=ints

all: $(PROGS)

ints:
	ghc --make $(GHC_OPTS) ints.hs	

clean:
	rm -f $(PROGS) *.o *.hi *~



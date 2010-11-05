.PHONY: clean bench

GHC_OPTS=-O2 -funbox-strict-fields
PROGS=bench

all: $(PROGS)

bench:
	ghc --make $(GHC_OPTS) bench.hs	

clean:
	rm -f $(PROGS) *.o *.hi *~



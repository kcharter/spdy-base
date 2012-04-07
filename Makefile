# This is just for the test programs.

.PHONY: all clean

GHC=ghc
GHC_FLAGS=-odir tmp -hidir tmp -O -package-conf cabal-dev/packages-7.0.4.conf
PROGS=sping sget

all: $(PROGS)

sping: SPing.hs
	$(GHC) $(GHC_FLAGS) --make SPing -main-is SPing -o sping

sget: SGet.hs
	$(GHC) $(GHC_FLAGS) --make SGet -main-is SGet -o sget

clean:
	rm -rf $(PROGS) tmp

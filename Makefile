# This is just for the test programs.

.PHONY: all clean

GHC=ghc
GHC_FLAGS=-odir tmp -hidir tmp -O -package-conf cabal-dev/packages-7.0.4.conf -hide-package monads-tf
PROGS=sping sget sget-prof tls-server tls-client

all: $(PROGS)

sping: SPing.hs
	$(GHC) $(GHC_FLAGS) --make SPing -main-is SPing -o sping

sget: SGet.hs
	$(GHC) $(GHC_FLAGS) --make SGet -main-is SGet -o sget

sget-prof: SGet.hs
	$(GHC) $(GHC_FLAGS) --make SGet -main-is SGet -o sget-prof -rtsopts -prof -auto-all -caf-all

tls-server: tls-server.hs
	$(GHC) $(GHC_FLAGS) --make tls-server -main-is TlsServer -o tls-server

tls-client: tls-client.hs
	$(GHC) $(GHC_FLAGS) --make tls-client -main-is TlsClient -o tls-client

clean:
	rm -rf $(PROGS) tmp

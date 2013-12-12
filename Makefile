TARGETS = test-integer-simple test-integer-gmp

GHC = ghc
GHCVER = $(shell $(GHC) --version | sed "s/.* //")
GHCFLAGS = -Wall $(PACKAGES) $(PRAGMAS)

PRAGMAS = -XCPP -XMagicHash -XUnboxedTuples -XUnliftedFFITypes


SIMPLE = -i:integer-simple
GMP = -i:integer-gmp


all : $(TARGETS)


test-integer-simple : test-integer.hs Support.hs
	$(GHC) $(GHCFLAGS) --make $(SIMPLE) $< -o $@


test-integer-gmp : test-integer.hs Support.hs
	$(GHC) $(GHCFLAGS) --make $(GMP) $< -o $@


update :
	(cd integer-gmp && git checkout master && git pull --rebase && git checkout ghc-$(GHCVER)-release)
	(cd integer-simple && git checkout master && git pull --rebase && git checkout ghc-$(GHCVER)-release)


clean :
	@rm -f $(TARGETS)
	@find . -name \*.o -o -name \*.hi | xargs rm -f


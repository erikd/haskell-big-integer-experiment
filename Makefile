TARGETS = test-integer-simple test-integer-gmp

GHC = ghc -Wall $(PACKAGES) $(PRAGMAS)

PRAGMAS = -XUnliftedFFITypes

SIMPLE = -i:integer-simple
GMP = -i:integer-gmp


all : $(TARGETS)


test-integer-simple : test-integer.hs Support.hs
	$(GHC) --make $(SIMPLE) $< -o $@


test-integer-gmp : test-integer.hs Support.hs
	$(GHC) --make $(GMP) $< -o $@


update :
	(cd integer-gmp && git pull --rebase)
	(cd integer-simple && git pull --rebase)


clean :
	@rm -f $(TARGETS)
	@find . -name \*.o -o -name \*.hi | xargs rm -f


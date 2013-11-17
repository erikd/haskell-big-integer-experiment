
GHC = ghc -Wall $(PACKAGES) $(PRAGMAS)

PRAGMAS = -XCPP -XMagicHash -XBangPatterns -XUnboxedTuples -XForeignFunctionInterface -XUnliftedFFITypes

SIMPLE = -i:integer-simple
GMP = -i:integer-gmp


all : test-integer-simple test-integer-gmp


test-integer-simple : test-integer.hs
	$(GHC) --make $(SIMPLE) $< -o $@


test-integer-gmp : test-integer.hs
	$(GHC) --make $(GMP) $< -o $@


update :
	(cd integer-gmp && git pull --rebase)
	(cd integer-simple && git pull --rebase)


clean :
	@find . -name \*.o -o -name \*.hi | xargs rm -f


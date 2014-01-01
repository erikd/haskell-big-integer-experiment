TARGETS = check-integer bench-integer new-bench-integer

GHC = ghc
GHCVER = $(shell $(GHC) --version | sed "s/.* //")
GHCFLAGS = -Wall -fwarn-tabs -Werror -O3 $(PACKAGES) $(PRAGMAS)

hsfiles = $(shell find Check/ GMP/ New*/ Simple/ -name \*.hs -o -name \*.lhs) *.hs

PRAGMAS = -XCPP -XMagicHash -XUnboxedTuples -XUnliftedFFITypes


SIMPLE = -i:integer-simple
GMP = -i:integer-gmp


all : $(TARGETS)

bench : bench-integer
	./bench-integer -o bench-integer.html

new-bench : new-bench-integer
	./new-bench-integer --no-gc -o new-bench-integer.html --template=Criterion/report.tpl
	chmod a+r new-bench-integer.html

check : check-integer
	./check-integer # | tee check.log

check-integer : check-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $(SIMPLE) $< -o $@

bench-integer : bench-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $(SIMPLE) $< -o $@

new-bench-integer : new-bench-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $(SIMPLE) $< -o $@

int-bench : int-bench.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $(SIMPLE) $< -o $@

Stamp/update :
	@if test ! -d integer-gmp ; then \
		git clone http://git.haskell.org/packages/integer-gmp.git ; \
		fi
	@if test ! -d integer-simple ; then \
		git clone http://git.haskell.org/packages/integer-simple.git ; \
		fi
	(cd integer-gmp && git checkout master && git pull --rebase)
	(cd integer-simple && git checkout master && git pull --rebase)
	@touch $@

Stamp/version-$(GHCVER) : Stamp/update
	(cd integer-gmp && ../Scripts/git-branch-tag.sh ghc-$(GHCVER)-release && git checkout ghc-$(GHCVER)-release)
	(cd integer-simple && ../Scripts/git-branch-tag.sh ghc-$(GHCVER)-release && git checkout ghc-$(GHCVER)-release)
	@touch $@

Stamp/copy : Stamp/version-$(GHCVER)
	Scripts/copy_modify.sh integer-simple Simple
	Scripts/copy_modify.sh integer-gmp GMP
	@touch $@

clean :
	@rm -f $(TARGETS)
	@find . -name \*.o -o -name \*.hi | xargs rm -f

realclean :
	@rm -f Stamp/*

TARGETS = check-integer bench-integer new-bench-integer

GHC = ghc
GHCFLAGS = -Wall -fwarn-tabs -Werror -O3 $(PRAGMAS)

hsfiles = $(shell find Check/ GMP/ New*/ Simple/ -name \*.hs -o -name \*.lhs) *.hs $(checkfiles)

gmp_cmm_files = -IGMP GMP/gmp-wrappers.cmm

checkfiles = Check/New1.hs Check/New2.hs Check/New3.hs

PRAGMAS = -XCPP -XMagicHash -XUnboxedTuples -XUnliftedFFITypes

BROWSER ?= firefox


all : $(TARGETS)

check : check-integer
	./check-integer # | tee check.log

dcheck : div-check
	./div-check

core3 :
	ghc-core New3/GHC/Integer/Internals.hs

asm3 :
	$(GHC) $(GHCFLAGS) -DTESTING -keep-s-files New3/GHC/Integer/Internals.hs
	less New3/GHC/Integer/Internals.s

llvm3 :
	$(GHC) $(GHCFLAGS) -DTESTING -fllvm -keep-llvm-files New3/GHC/Integer/Internals.hs
	less New3/GHC/Integer/Internals.ll

check-integer : check-integer.hs Stamp/copy $(hsfiles) Check/New1.hs Check/New2.hs Check/New3.hs
	$(GHC) $(GHCFLAGS) -DTESTING --make $< $(gmp_cmm_files) -o $@

div-check : div-check.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) -DTESTING --make $< $(gmp_cmm_files) -o $@

bench-integer : bench-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

new-bench-integer : new-bench-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< $(gmp_cmm_files) -o $@

times-bench : times-bench.hs $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

times-check : times-check.hs $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

int-bench : int-bench.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

karatsubaSlice : karatsubaSlice.hs $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

karatsubaSlice2 : karatsubaSlice2.hs $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

karatsubaSlice3 : karatsubaSlice3.hs $(hsfiles)
	$(GHC) -DDEBUG=0 $(GHCFLAGS) --make $< -o $@

karatsubaSlice3D : karatsubaSlice3.hs $(hsfiles)
	$(GHC) -DDEBUG=1 $(GHCFLAGS) --make $< -o $@

kslice2 : karatsubaSlice2
	./$+

kslice : karatsubaSlice3
	./$+

ksliced : karatsubaSlice3D
	./$+


Check/New1.hs : Check/NewX.hs.tpl
	sed "s/NewX/New1/;s/###//g" $+ > $@

Check/New2.hs : Check/NewX.hs.tpl
	sed "s/NewX/New2/;s/###//g" $+ > $@

Check/New3.hs : Check/NewX.hs.tpl
	sed "s/NewX/New3/;s/###/#/g" $+ > $@

view-bench : new-bench-integer
	./new-bench-integer --no-gc -o new-bench-integer.html --template=Criterion/report.tpl
	chmod a+r new-bench-integer.html
	$(BROWSER) new-bench-integer.html

view-times : times-bench times-check
	./times-check
	./times-bench --no-gc -o times-bench.html --template=Criterion/report.tpl
	$(BROWSER) times-bench.html

# Update the local copies of integer-simple and integer-gmp and patch them
# to work in this framework.
update :
	rm -f Stamp/*
	make Stamp/copy

Stamp/update :
	mkdir -p Stamp
	@if test ! -d integer-gmp ; then \
		git clone http://git.haskell.org/packages/integer-gmp.git ; \
		fi
	@if test ! -d integer-simple ; then \
		git clone http://git.haskell.org/packages/integer-simple.git ; \
		fi
	(cd integer-gmp && git checkout master && git pull --rebase)
	(cd integer-simple && git checkout master && git pull --rebase)
	@touch $@

Stamp/copy : Stamp/update  Stamp/ghc-version
	Scripts/copy_modify.sh integer-simple Simple
	Scripts/copy_modify.sh integer-gmp GMP
	cp integer-gmp/cbits/gmp-wrappers.cmm GMP/
	@touch $@

Stamp/ghc-version :
	Scripts/ghc-version.sh
	touch $@


clean :
	@rm -f $(TARGETS) Check/New*.hs
	@find . -name \*.o -o -name \*.hi -o -name \*.s -o -name \*.ll -o -name \*.hcr | xargs rm -f

realclean :
	@rm -f Stamp/*

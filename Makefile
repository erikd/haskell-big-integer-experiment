TARGETS = check-integer bench-integer

GHC = cabal exec -- ghc
GHCFLAGS = -Wall -Werror -fwarn-tabs -funbox-strict-fields -fPIC -O3 $(PRAGMAS)

ghc_major_version = $(shell ghc --numeric-version | sed 's/\..*//')

hsdirs = Common/ Check/ GMP/ New*/ Simple/

hsfiles = $(shell find $(hsdirs) -name \*.hs -o -name \*.lhs) *.hs $(checkfiles)

bench_hsfiles = Check/BenchG.hs Check/Bench1.hs Check/Bench2.hs Check/Bench3.hs Check/Bench4.hs

checkfiles = Check/New1.hs Check/New2.hs Check/New3.hs Check/New4.hs

today := $(shell date "+%Y%m%d")

PRAGMAS = -XCPP -XMagicHash -XUnboxedTuples -XUnliftedFFITypes

BROWSER ?= firefox -no-remote


all : $(TARGETS)

check : check-integer test-internals
	./test-internals
	./check-integer # | tee check.log

core3 :
	ghc-core New3/GHC/Integer/Internals.hs

asm3 :
	$(GHC) $(GHCFLAGS) -keep-s-files New3/GHC/Integer/Internals.hs
	less New3/GHC/Integer/Internals.s

llvm3 :
	$(GHC) $(GHCFLAGS) -fllvm -keep-llvm-files New3/GHC/Integer/Internals.hs
	less New3/GHC/Integer/Internals.ll

check-integer : check-integer.hs Stamp/ready $(hsfiles) Check/New1.hs Check/New2.hs Check/New3.hs Check/New4.hs
	$(GHC) $(GHCFLAGS) --make $< -o $@

test-internals : test-internals.hs $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

bench-integer : bench-integer.hs Stamp/ready $(hsfiles) $(bench_hsfiles)
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


Check/New1.hs : Check/NewX.template.hs
	sed "s/NewX/New1/" $+ > $@

Check/New2.hs : Check/NewX.template.hs
	sed "s/NewX/New2/" $+ > $@

Check/New3.hs : Check/NewX.template.hs
	sed "s/NewX/New3/" $+ > $@

Check/New4.hs : Check/NewX.template.hs
	sed "s/NewX/New4/" $+ > $@


Check/BenchG.hs : Check/BenchX.template.hs
	sed "s/BenchX/BenchG/;s/NewX.GHC/GMP.GHC$(ghc_major_version)/" $+ > $@

Check/Bench1.hs : Check/BenchX.template.hs
	sed "s/BenchX/Bench1/;s/NewX/New1/" $+ > $@

Check/Bench2.hs : Check/BenchX.template.hs
	sed "s/BenchX/Bench2/;s/NewX/New2/" $+ > $@

Check/Bench3.hs : Check/BenchX.template.hs
	sed "s/BenchX/Bench3/;s/NewX/New3/" $+ > $@

Check/Bench4.hs : Check/BenchX.template.hs
	sed "s/BenchX/Bench4/;s/NewX/New4/" $+ > $@


bench-integer.html : bench-integer Criterion/report.tpl
	./bench-integer --no-gc -o bench-integer.html --template=Criterion/report.tpl
	chmod a+r bench-integer.html


Criterion/report.tpl : Criterion/report.tpl.in
	sed "s/@GHC_VERSION@/$$(ghc --numeric-version)/;s/@OS_NAME@/$$(uname -s | tr 'A-Z' 'a-z')/g;s/@CPU_NAME@/$$(uname -m)/g" $< > $@



date-bench : bench-integer.html
	cp $< bench-integer-$(today).html

view-bench : bench-integer.html
	$(BROWSER) bench-integer.html

GMP/GmpDerivedConstants.h : integer-gmp-7.10/mkGmpDerivedConstants/mkGmpDerivedConstants.c
	gcc -Wall $< -o mkGmpDerivedConstants
	./mkGmpDerivedConstants > $@
	rm -f mkGmpDerivedConstants

# Update the local copies of integer-simple and integer-gmp and patch them
# to work in this framework.
update :
	rm -f Stamp/*
	make Stamp/copy

Stamp/update :
	mkdir -p Stamp
	@if test ! -d integer-gmp-7.10 ; then \
		git clone http://git.haskell.org/packages/integer-gmp.git integer-gmp-7.10 ; \
		fi
	@if test ! -d integer-simple ; then \
		git clone http://git.haskell.org/packages/integer-simple.git ; \
		fi
	(cd integer-gmp-7.10 && git checkout master && git pull --rebase)
	(cd integer-simple && git checkout master && git pull --rebase)
	@touch $@

Stamp/copy : Stamp/update  Stamp/ghc-version
	Scripts/copy_modify.sh integer-simple Simple
	Scripts/copy_modify.sh integer-gmp-7.10 GMP
	cp integer-gmp-7.10/cbits/gmp-wrappers.cmm GMP/
	@touch $@

Stamp/ready : Stamp/copy GMP/GmpDerivedConstants.h
	@touch $@

Stamp/ghc-version :
	Scripts/ghc-version.sh
	touch $@


clean :
	@rm -f $(TARGETS) *.o *.hi bench-integer.html Check/Bench[GS0-9].hs Check/New[0-9].hs
	@rm -f $(TARGETS) bench-integer.html Check/Bench[GS0-9].hs Check/New[0-9].hs
	@rm -f GMP/GmpDerivedConstants.h Criterion/report.tpl
	@find $(hsdirs) -name \*.o -o -name \*.hi -o -name \*.s -o -name \*.ll -o -name \*.hcr | xargs rm -f

hlint :
	hlint Common/ Check/ New*/

realclean :
	@rm -f Stamp/*

sandbox-reinit:
	cabal sandbox delete
	cabal sandbox init
	cabal install criterion hspec primitive

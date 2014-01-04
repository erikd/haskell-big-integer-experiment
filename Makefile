TARGETS = check-integer bench-integer new-bench-integer

GHC = ghc
GHCFLAGS = -Wall -fwarn-tabs -Werror -O3 $(PACKAGES) $(PRAGMAS)

hsfiles = $(shell find Check/ GMP/ New*/ Simple/ -name \*.hs -o -name \*.lhs) *.hs $(checkfiles)

checkfiles = Check/New1.hs Check/New2.hs Check/New3.hs

PRAGMAS = -XCPP -XMagicHash -XUnboxedTuples -XUnliftedFFITypes


all : $(TARGETS)

check : check-integer
	./check-integer # | tee check.log

check-integer : check-integer.hs Stamp/copy $(hsfiles) Check/New1.hs Check/New2.hs Check/New3.hs
	$(GHC) $(GHCFLAGS) --make $< -o $@

bench-integer : bench-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

new-bench-integer : new-bench-integer.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

int-bench : int-bench.hs Stamp/copy $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

karatsubaSlice : karatsubaSlice.hs $(hsfiles)
	$(GHC) $(GHCFLAGS) --make $< -o $@

kslice : karatsubaSlice
	./karatsubaSlice

new-bench : new-bench-integer
	./new-bench-integer --no-gc -o new-bench-integer.html --template=Criterion/report.tpl
	chmod a+r new-bench-integer.html

Check/New1.hs : Check/NewX.hs.tpl
	sed "s/NewX/New1/;s/###//g" $+ > $@

Check/New2.hs : Check/NewX.hs.tpl
	sed "s/NewX/New2/;s/###//g" $+ > $@

Check/New3.hs : Check/NewX.hs.tpl
	sed "s/NewX/New3/;s/###/#/g" $+ > $@

view-bench : new-bench-integer.html
	make new-bench
	gnome-www-browser $<

upload : new-bench-integer.html
	scp -p $< mega-nerd.net:/home/www.mega-nerd.com/data/tmp/

update :
	rm -f Stamp/*
	make Stamp/copy

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

Stamp/copy : Stamp/update
	Scripts/copy_modify.sh integer-simple Simple
	Scripts/copy_modify.sh integer-gmp GMP
	@touch $@

clean :
	@rm -f $(TARGETS) Check/New*.hs
	@find . -name \*.o -o -name \*.hi | xargs rm -f

realclean :
	@rm -f Stamp/*

# haskell-big-integer-experiment

This project aims to compare and contrast the two exisiting Haskell big
integer implementations (integer-simple and integer-gmp) to a a new
implementation that will hopefully replace the other two.

The reasons why a replacement for the above two is needed is explained
[here][reason].

The Git checkout for this project contains copies of the integer-simple and
integer-gmp trees (with some slight mods) as well as a number of alternative
implementations of the new library. When a decent implementation for the new
integer library emerges from this work it will be extracted and submitted for
inclusion in the GHC tree.

__This is still very much a work in progress. Patches and pull requests more
than welcome.__

## Benchmarks

Benchmarks comparing the four versions of this library with integer-gmp and
integer-simple are available [here][benchmarks].


## Requirements

Since we need to benchmark against the very latest versions of the integer-simple
and inetger-gmp we need to run the latest versions of GHC which at the time of
writing is 7.10.x (other versions will not work).

GNU Make is used for the build system.

The project is designed to use a cabal sandbox. The sandbox can be set up (and
re-initialialized) using:

	make sandbox-reinit

## Kicking the tires

A real good place to start with this is running the test suite which is as
easy as:

	make check

To run the crierion benchmarks  using:

    make view-bench

which will run the bench marks and display them in firefox or whatever browser
you set with the $BROWSER environment variable.


## TODO

This is the TODO list, with highest priority first.

* Make it work with ghc-8.0 (work in progress).
* Make it work on 32 bit CPUs (work in progress).
* Improve Big Integer multiplication.
* Benchmark division operations.


[reason]: http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/integer_pt1.html
[benchmarks]: http://www.mega-nerd.com/haskell-big-integer-experiment/

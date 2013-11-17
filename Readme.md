Start of a test suite and benchmarking suite for the two GHC big integer
libraries integer-simple and integer-gmp.

These libraries make use of low level GHC internal data structures and hence
are not portable across GHC versions.

The current test is aimed at ghc-7.6.3.

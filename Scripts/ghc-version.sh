#!/bin/bash

version=$(ghc --numeric-version | sed 's/\.[0-9]$//')

if test ${version} != "7.10" ; then
	echo "Error : We need GHC 7.10.* for this."
	exit 1
	fi

exit 0

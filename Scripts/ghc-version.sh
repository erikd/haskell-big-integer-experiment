#!/bin/bash

version=$(ghc --numeric-version | sed 's/\.[0-9]$//')

echo $version

if test ${version} != "7.8" ; then
	echo "Error : We need GHC 7.8.* for this."
	exit 1
	fi

exit 0

#!/bin/bash

version=$(ghc --numeric-version)

case "${version}" in
	7.10.*)
		;;
	8.0.*)
		echo "GHC version ${version} may not fully work."
		;;
	*)
		echo "Error : We need GHC > 7.10.* for this."
		exit 1
		;;
	esac

exit 0

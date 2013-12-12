#!/bin/bash -e

if test $# -ne 2 ; then
	echo "$0 needs two command line arguments."
	exit 1
	fi

for fromfile in $(find $1/GHC -name \*.hs -o -name \*.lhs) ; do
	tofile=$(echo $fromfile | sed "s/$1/$2/")
	mkdir -p $(dirname $tofile)
	sed "s/GHC.Integer/$2.GHC.Integer/" ${fromfile} > ${tofile}
	done

#!/bin/bash

if test $(git branch | grep -c $1) -ne 1 ; then
	git checkout -b newbranch $1
	fi

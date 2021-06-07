#!/bin/sh

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

# Compile tests
gcc -o test_util.out -I../src test_util.c ../src/util.c -lm

# Run and delete tests
for TEST in *.out; do
	./"$TEST"
	rm "$TEST"
done

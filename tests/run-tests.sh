#!/bin/sh

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

# Compile C tests

# Compile C++ tests
g++ -o test_Entity.out -I../src test_Entity.cpp ../src/Entity.cpp ../src/Vec2.cpp -lm

# Run and delete all tests
for TEST in *.out; do
	./"$TEST"
	rm "$TEST"
done

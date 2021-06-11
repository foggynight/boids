#!/bin/sh

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

# Compile C tests

# Compile C++ tests
g++ -o test_Entity.out -I../src test_Entity.cpp approx_equal.c ../src/util.c ../src/Entity.cpp ../src/Vec2.cpp -lm
g++ -o test_Vec2.out -I../src test_Vec2.cpp approx_equal.c ../src/Vec2.cpp -lm

# Run and delete all tests
for TEST in *.out; do
	./"$TEST"
	rm "$TEST"
done

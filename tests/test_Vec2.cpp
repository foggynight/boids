// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <cmath>
#include <iostream>

#include "Vec2.hpp"

extern "C" {
#include "approx_equal.h"
}

void test_angle();
void test_length();
void test_normalize();

int main()
{
	test_angle();
	test_length();
	test_normalize();
	std::cout << "test_Vec2: All tests passed" << std::endl;
	return 0;
}

void test_angle()
{
	Vec2 vec;
	auto test = [&vec](float x, float y, float angle) {
		vec.x = x;
		vec.y = y;
		assert(approx_equal(vec.angle(), angle));
	};

	// @TODO Add test_angle test cases
}

void test_length()
{
	Vec2 vec;
	auto test = [&vec](float x, float y, float length) {
		vec.x = x;
		vec.y = y;
		assert(approx_equal(vec.length(), length));
	};

	test(0, 0, 0);
	test(1, 0, 1);
	test(0, 1, 1);
	test(-1, 0, 1);
	test(0, -1, 1);
	test(1, 1, sqrt(2));
	test(-1, 1, sqrt(2));
	test(-1, -1, sqrt(2));
	test(1, -1, sqrt(2));
}

void test_normalize()
{
	Vec2 vec;
	auto test = [&vec](float x, float y, float new_x, float new_y) {
		vec.x = x;
		vec.y = y;
		vec.normalize();
		assert(approx_equal(vec.x, new_x));
		assert(approx_equal(vec.y, new_y));
	};

	test(0, 0, 1, 0);
	test(2, 0, 1, 0);
	test(0, 2, 0, 1);
	test(-2, 0, -1, 0);
	test(0, -2, 0, -1);
	test(1, 1, 1/sqrt(2), 1/sqrt(2));
	test(-1, 1, -1/sqrt(2), 1/sqrt(2));
	test(-1, -1, -1/sqrt(2), -1/sqrt(2));
	test(1, -1, 1/sqrt(2), -1/sqrt(2));
}

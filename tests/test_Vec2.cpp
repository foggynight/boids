// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <cmath>
#include <iostream>

#include "Vec2.hpp"

extern "C" {
#include "approx_equal.h"
}

void test_to_unit_vector()
{
	/**
	 * Test case format: input_x, input_y, expected_x, expected_y
	 **/

	Vec2 vec(0.0f, 0.0f);

	// Test 0: 0, 0, 1, 0
	vec.to_unit_vector();
	assert(approx_equal(vec.x, 1.0f));
	assert(approx_equal(vec.y, 0.0f));

	// Test 1: 2, 0, 1, 0
	vec.x = 2.0f;
	vec.y = 0.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, 1.0f));
	assert(approx_equal(vec.y, 0.0f));

	// Test 2: 0, 2, 0, 1
	vec.x = 0.0f;
	vec.y = 2.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, 0.0f));
	assert(approx_equal(vec.y, 1.0f));

	// Test 3: -2, 0, -1, 0
	vec.x = -2.0f;
	vec.y = 0.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, -1.0f));
	assert(approx_equal(vec.y, 0.0f));

	// Test 4: 0, -2, 0, -1
	vec.x = 0.0f;
	vec.y = -2.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, 0.0f));
	assert(approx_equal(vec.y, -1.0f));

	// Test 5: 1, 1, 1/sqrt(2), 1/sqrt(2)
	vec.x = 1.0f;
	vec.y = 1.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, (float)1/sqrt(2.0)));
	assert(approx_equal(vec.y, (float)1/sqrt(2.0)));

	// Test 6: -1, 1, -1/sqrt(2), 1/sqrt(2)
	vec.x = -1.0f;
	vec.y = 1.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, (float)-1/sqrt(2.0)));
	assert(approx_equal(vec.y, (float)1/sqrt(2.0)));

	// Test 7: -1, -1, -1/sqrt(2), -1/sqrt(2)
	vec.x = -1.0f;
	vec.y = -1.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, (float)-1/sqrt(2.0)));
	assert(approx_equal(vec.y, (float)-1/sqrt(2.0)));

	// Test 8: 1, -1, 1/sqrt(2), -1/sqrt(2)
	vec.x = 1.0f;
	vec.y = -1.0f;
	vec.to_unit_vector();
	assert(approx_equal(vec.x, (float)1/sqrt(2.0)));
	assert(approx_equal(vec.y, (float)-1/sqrt(2.0)));
}

int main()
{
	test_to_unit_vector();
	std::cout << "test_Vec2: All tests passed" << std::endl;
	return 0;
}

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <assert.h>
#include <stdio.h>

#include "util.h"

void test_find_shorter_rotation(void);

int main(void)
{
	test_find_shorter_rotation();
	printf("test_util: All tests passed\n");
	return 0;
}

void test_find_shorter_rotation(void)
{
	/**
	 * Test case format: start_angle, end_angle, expected_result
	 **/

	// Test 0: 0, 0, NO_ROT
	assert(find_shorter_rotation(0.0f, 0.0f) == NO_ROT);

	// Test 1: 0, 90, CW
	assert(find_shorter_rotation(0.0f, 90.0f) == CW);

	// Test 2: 0, 180, CW
	assert(find_shorter_rotation(0.0f, 180.0f) == CW);

	// Test 3: 0, 270, CCW
	assert(find_shorter_rotation(0.0f, 270.0f) == CCW);

	// Test 4: 270, 0, CW
	assert(find_shorter_rotation(270.0f, 0.0f) == CW);

	// Test 5: 270, 90, CW
	assert(find_shorter_rotation(270.0f, 90.0f) == CW);

	// Test 6: 270, 180, CW
	assert(find_shorter_rotation(270.0f, 180.0f) == CCW);
}

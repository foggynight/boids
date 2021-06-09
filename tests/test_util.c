// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <assert.h>
#include <stdio.h>

#include "util.h"

void test_get_vec2_angle(void);

int main(void)
{
	test_get_vec2_angle();
	printf("test_util: All tests passed\n");
	return 0;
}

void test_get_vec2_angle(void)
{
	/**
	 * Test case format: delta_x, delta_y, expected_result
	 **/

	// Test 0: 0, 0, 0
	assert(get_vec2_angle(0, 0) == 0.0f);

	// Test 1: 1, 0, 0
	assert(get_vec2_angle(1, 0) == 0.0f);

	// Test 2: 1, 1, 45
	assert(get_vec2_angle(1, 1) == 45.0f);

	// Test 3: 0, 1, 90
	assert(get_vec2_angle(0, 1) == 90.0f);

	// Test 4: -1, 1, 135
	assert(get_vec2_angle(-1, 1) == 135.0f);

	// Test 5: -1, 0, 180
	assert(get_vec2_angle(-1, 0) == 180.0f);

	// Test 6: -1, -1, 225
	assert(get_vec2_angle(-1, -1) == 225.0f);

	// Test 7: 0, -1, 270
	assert(get_vec2_angle(0, -1) == 270.0f);

	// Test 8: 1, -1, 315
	assert(get_vec2_angle(1, -1) == 315.0f);
}

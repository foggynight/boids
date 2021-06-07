// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <assert.h>
#include <stdio.h>

#include "util.h"

void test_get_delta_position_angle(void);

int main(void)
{
	test_get_delta_position_angle();
	printf("test_util: All tests passed\n");
	return 0;
}

/**
 * Test case format: delta_x, delta_y, expected_result
 **/
void test_get_delta_position_angle(void)
{
	// Test 0: 0, 0, 0
	assert(get_delta_position_angle(0, 0) == 0);

	// Test 1: 1, 0, 0
	assert(get_delta_position_angle(1, 0) == 0);

	// Test 2: 1, 1, 45
	assert(get_delta_position_angle(1, 1) == 45);

	// Test 3: 0, 1, 90
	assert(get_delta_position_angle(0, 1) == 90);

	// Test 4: -1, 1, 135
	assert(get_delta_position_angle(-1, 1) == 135);

	// Test 5: -1, 0, 180
	assert(get_delta_position_angle(-1, 0) == 180);

	// Test 6: -1, -1, 225
	assert(get_delta_position_angle(-1, -1) == 225);

	// Test 7: 0, -1, 270
	assert(get_delta_position_angle(0, -1) == 270);

	// Test 8: 1, -1, 315
	assert(get_delta_position_angle(1, -1) == 315);
}

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
	assert(find_shorter_rotation(0.0f, 0.0f) == NO_ROT);
	assert(find_shorter_rotation(0.0f, 90.0f) == CW);
	assert(find_shorter_rotation(0.0f, 180.0f) == CW);
	assert(find_shorter_rotation(0.0f, 270.0f) == CCW);
	assert(find_shorter_rotation(270.0f, 0.0f) == CW);
	assert(find_shorter_rotation(270.0f, 90.0f) == CW);
	assert(find_shorter_rotation(270.0f, 180.0f) == CCW);
}

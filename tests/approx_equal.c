// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

#include "approx_equal.h"

#define APPROX_EQUAL_THRESHOLD	0.000001f

int approx_equal(float x, float y)
{
	float diff = fabs(y - x);
	return diff <= APPROX_EQUAL_THRESHOLD;
}

#ifdef __cplusplus
}
#endif

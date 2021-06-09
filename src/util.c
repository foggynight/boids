// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

// Not a fan of using a C++ compiler for C, but this makes writing the test
// script easier so I will do it for now.
#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

#include "util.h"

float get_vec2_angle(float x, float y)
{
	float theta = 0.0f;

	if (x == 0.0f && y == 0.0f)
		theta = 0.0f;
	else if (x == 0.0f) {
		if (y > 0.0f)
			theta = 90.0f;
		else	// y < 0.0f
			theta = 270.0f;
	}
	else if (y == 0.0f) {
		if (x > 0.0f)
			theta = 0.0f;
		else	// x < 0.0f
			theta = 180.0f;
	}
	else {
		theta = rad_to_deg(atan(y / x));	// Already determined that x != 0.0f
		if (x < 0.0f)
			theta += 180.0f;
		else if (y < 0.0f)
			theta += 360.0f;
	}

	return theta;
}

float get_vec2_length(float x, float y)
{
	return sqrt(pow(x, 2) + pow(y, 2));
}

#ifdef __cplusplus
}
#endif

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

#include "util.h"

float get_delta_position_angle(float delta_x, float delta_y)
{
	float phi = 0.0f;	// Shorthand for delta_position_angle

	if (delta_x == 0.0f && delta_y == 0.0f)
		phi = 0.0f;
	else if (delta_x == 0.0f) {
		if (delta_y > 0.0f)
			phi = 90.0f;
		else	// delta_y < 0.0f
			phi = 270.0f;
	}
	else if (delta_y == 0.0f) {
		if (delta_x > 0.0f)
			phi = 0.0f;
		else	// delta_x < 0.0f
			phi = 180.0f;
	}
	else {
		phi = rad_to_deg(atan(delta_y / delta_x));	// Already determined that delta_x != 0.0f
		if (delta_x < 0.0f)
			phi += 180.0f;
		else if (delta_y < 0.0f)
			phi += 360.0f;
	}

	return phi;
}

#ifdef __cplusplus
}
#endif

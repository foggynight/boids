// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifdef __cplusplus
extern "C" {
#endif

#include "util.h"

int find_shorter_rotation(float start_angle, float end_angle)
{
		float cw_angle, ccw_angle;
		if (start_angle < end_angle) {
			cw_angle = end_angle - start_angle;
			ccw_angle = 360.0f - end_angle + start_angle;
		}
		else {	// start_angle > end_angle
			cw_angle = start_angle - end_angle;
			ccw_angle = 360.0f - start_angle + end_angle;
		}
		return cw_angle > ccw_angle ? -1 : 1;
}

#ifdef __cplusplus
}
#endif

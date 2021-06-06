// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef UTIL_H
#define UTIL_H

#define US_PER_SECOND	1000000	// Microseconds per second

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)	// Convert degrees to radians
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)	// Convert radians to degrees

float get_delta_position_angle(float delta_x, float delta_y);

#endif	// UTIL_H

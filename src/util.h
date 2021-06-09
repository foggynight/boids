// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef UTIL_H
#define UTIL_H

#define US_PER_SECOND	1000000	// Microseconds per second

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)	// Convert degrees to radians
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)	// Convert radians to degrees

/**
 * Get the angle of a vector with components delta_x and delta_y, relative to
 * the right facing x-axis with a clockwise rotation starting at the x-axis.
 *
 * @param delta_x	x component of the vector
 * @param delta_y	y component of the vector
 *
 * @return Angle of the vector
 **/
float get_vec2_angle(float delta_x, float delta_y);

#endif	// UTIL_H

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef UTIL_H
#define UTIL_H

#define US_PER_SECOND	1000000	// Microseconds per second

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)	// Convert degrees to radians
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)	// Convert radians to degrees

/**
 * Find the shorter direction for start_angle to be rotated to be equal to
 * end_angle.
 *
 * @param start_angle	Angle to start rotation at
 * @param end_angle	Angle to end rotation at
 *
 * @return 1 if clockwise else -1
 *
 * @note If the rotation directions are equal in length, clockwise is chosen by
 * default.
 **/
int find_shorter_rotation(float start_angle, float end_angle);

#endif	// UTIL_H

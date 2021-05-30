// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cmath>

#include "Entity.hpp"
#include "screen.hpp"

#define MICROSECONDS_PER_SECOND	1000000

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)	// Convert degrees to radians
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)	// Convert radians to degrees

void Entity::update_pos(int delta_time_us)
{
	const float w_rotated = fabs((double)w * cos(deg_to_rad(angle))) + fabs((double)h * sin(deg_to_rad(angle)));
	const float h_rotated = fabs((double)w * sin(deg_to_rad(angle))) + fabs((double)h * cos(deg_to_rad(angle)));

	const float w_rotated_radius = w_rotated / 2.0f;
	const float h_rotated_radius = h_rotated / 2.0f;

	x += velocity * (float)cos(deg_to_rad(angle)) * (float)delta_time_us / (float)MICROSECONDS_PER_SECOND;
	y += velocity * (float)sin(deg_to_rad(angle)) * (float)delta_time_us / (float)MICROSECONDS_PER_SECOND;

	if (x < -w_rotated_radius)
		x = (float)(WIN_WIDTH-1) + w_rotated_radius;
	else if (x > (float)(WIN_WIDTH-1) + w_rotated_radius)
		x = -w_rotated_radius;

	if (y < -h_rotated_radius)
		y = (float)(WIN_HEIGHT-1) + h_rotated_radius;
	else if (y > (float)(WIN_HEIGHT-1) + h_rotated_radius)
		y = -h_rotated_radius;
}

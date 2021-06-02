// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cmath>

#include "Entity.hpp"
#include "screen.hpp"

extern "C" {
#include "util.h"
}

static float fov_radius = 256.0f;
static float fov_max_angle = 120.0f;

static float get_delta_position_angle(float delta_x, float delta_y);

void Entity::update_pos(int delta_time_us)
{
	x += velocity * (float)cos(deg_to_rad(angle)) * (float)delta_time_us / (float)US_PER_SECOND;
	y += velocity * (float)sin(deg_to_rad(angle)) * (float)delta_time_us / (float)US_PER_SECOND;

	const float w_rotated = fabs((double)w * cos(deg_to_rad(angle))) + fabs((double)h * sin(deg_to_rad(angle)));
	const float h_rotated = fabs((double)w * sin(deg_to_rad(angle))) + fabs((double)h * cos(deg_to_rad(angle)));

	const float w_rotated_radius = w_rotated / 2.0f;
	const float h_rotated_radius = h_rotated / 2.0f;

	if (x < -w_rotated_radius)
		x = (float)(WIN_WIDTH-1) + w_rotated_radius;
	else if (x > (float)(WIN_WIDTH-1) + w_rotated_radius)
		x = -w_rotated_radius;

	if (y < -h_rotated_radius)
		y = (float)(WIN_HEIGHT-1) + h_rotated_radius;
	else if (y > (float)(WIN_HEIGHT-1) + h_rotated_radius)
		y = -h_rotated_radius;
}

bool Entity::in_fov(Entity& target)
{
	const float delta_x = target.x - x;
	const float delta_y = target.y - y;

	// Distance between the positions of this and target
	const float distance = sqrt(pow(delta_x, 2) + pow(delta_y, 2));

	// Angle of the vector pointing from this to target
	const float delta_position_angle = get_delta_position_angle(delta_x, delta_y);

	// Difference in the angle of the vector pointing from this to
	// target and the forward angle of this
	const float delta_angle = fabs(delta_position_angle - angle);

	return distance <= fov_radius
		&& (delta_angle <= fov_max_angle || delta_angle >= 360.0f - fov_max_angle);
}

float Entity::get_fov_radius() { return fov_radius; }
float Entity::get_fov_max_angle() { return fov_max_angle; }

static float get_delta_position_angle(float delta_x, float delta_y)
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

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <cmath>

#include "Entity.hpp"
#include "screen.hpp"

extern "C" {
#include "util.h"
}

// START: Entity constants
static const int width = 32;	// Width in pixels
static const int height = 32;	// Height in pixels

static const float rotation_speed = 90.0f;	// Rotation speed in degrees/sec

static const float fov_radius = 256.0f;	// Radius of the FOV in pixels
static const float fov_max_angle = 120.0f;	// Maximum angle of the FOV in degrees, 0 <= fov_max_angle <= 180
// END: Entity constants

// START: Entity static functions
int Entity::get_width() { return width; }
int Entity::get_height() { return height; }

float Entity::get_fov_radius() { return fov_radius; }
float Entity::get_fov_max_angle() { return fov_max_angle; }
// END: Entity static functions

Entity::Entity(float x, float y, float angle, float velocity)
	: x(x), y(y), angle(angle), velocity(velocity) {}

void Entity::rotate(float delta_angle)
{
	float new_angle = angle + delta_angle;
	while (new_angle < 0.0f)
		new_angle += 360.0f;
	while (new_angle >= 360.0f)
		new_angle -= 360.0f;
	angle = new_angle;
}

void Entity::rotate_towards(float target_angle, int delta_time_us)
{
	assert(target_angle >= 0.0f && target_angle < 360.0f);

	if (angle != target_angle) {
		// @TODO Wrap finding the shorter rotation in a function

		float cw_angle, ccw_angle;
		if (angle < target_angle) {
			cw_angle = target_angle - angle;
			ccw_angle = 360.0f - target_angle + angle;
		}
		else {	// angle > target_angle
			cw_angle = angle - target_angle;
			ccw_angle = 360.0f - angle + target_angle;
		}

		float delta_angle;
		if (cw_angle > ccw_angle)
			delta_angle = -ccw_angle;
		else	// cw_angle <= ccw_angle
			delta_angle = cw_angle;

		float rotation_magnitude = rotation_speed * (float)delta_time_us / (float)US_PER_SECOND;
		rotate(delta_angle >= 0.0f ? 1 : -1 * rotation_magnitude);
	}
}

void Entity::update_pos(int delta_time_us)
{
	x += velocity * (float)cos(deg_to_rad(angle)) * (float)delta_time_us / (float)US_PER_SECOND;
	y += velocity * (float)sin(deg_to_rad(angle)) * (float)delta_time_us / (float)US_PER_SECOND;

	const float w_rotated = fabs((double)width * cos(deg_to_rad(angle))) + fabs((double)height * sin(deg_to_rad(angle)));
	const float h_rotated = fabs((double)width * sin(deg_to_rad(angle))) + fabs((double)height * cos(deg_to_rad(angle)));

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
	const float distance = get_vec2_length(delta_x, delta_y);

	// Angle of the vector pointing from this to target
	const float delta_position_angle = get_vec2_angle(delta_x, delta_y);

	// Difference in the angle of the vector pointing from this to target and
	// the forward angle of this
	const float delta_angle = fabs(delta_position_angle - angle);

	return distance <= fov_radius
		&& (delta_angle <= fov_max_angle || delta_angle >= 360.0f - fov_max_angle);
}

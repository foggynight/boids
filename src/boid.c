// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <math.h>
#include <time.h>

#include "boid.h"
#include "screen.h"

#define BOID_SIDE_LENGTH	32.0f	// Side length of the square containing a boid
#define BOID_ROTATION_RATE	90.0f	// Rotation rate in degrees per second

#define BOID_FOV_RADIUS	10.0f	// Radius of a boid's field of view -- Equivalent to a view distance
#define BOID_FOV_MAX_ANGLE	135.0f	// Max angle of a boid's field of view relative to its forward direction

#define ROTATED_SQUARE_OFFSET	0.7071f	// Ratio of side length to co-axial radius of a square rotated at 45 degrees

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)	// Convert degrees to radians
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)	// Convert radians to degrees

float boid_w = BOID_SIDE_LENGTH;	// Width of the square containing a boid
float boid_h = BOID_SIDE_LENGTH;	// Height of the square containing a boid

boid_t boid_arr[MAX_BOID_COUNT];
size_t boid_count;

static void boid_align(clock_t time_delta);
static float boid_calculate_mean_angle(void);

void boid_update(clock_t time_delta)
{
	float boid_wrap_offset_w = (float)boid_w * ROTATED_SQUARE_OFFSET;
	float boid_wrap_offset_h = (float)boid_h * ROTATED_SQUARE_OFFSET;

	boid_align(time_delta);

	for (size_t i = 0; i < boid_count; ++i) {
		boid_t *boid = &boid_arr[i];

		boid->x += boid->velocity * (float)cos(deg_to_rad(boid->angle));
		boid->y += boid->velocity * (float)sin(deg_to_rad(boid->angle));

		if (boid->x < -boid_wrap_offset_w)
			boid->x = (float)(WIN_WIDTH-1) + boid_wrap_offset_w;
		else if (boid->x >= (float)WIN_WIDTH + boid_wrap_offset_w)
			boid->x = -boid_wrap_offset_w;

		if (boid->y < -boid_wrap_offset_h)
			boid->y = (float)(WIN_HEIGHT-1) + boid_wrap_offset_h;
		else if (boid->y >= (float)WIN_HEIGHT + boid_wrap_offset_h)
			boid->y = -boid_wrap_offset_h;
	}
}

static void boid_align(clock_t time_delta)
{
	float time_delta_seconds = (float)time_delta / (float)CLOCKS_PER_SEC;
	float boid_mean_angle = boid_calculate_mean_angle();

	for (size_t i = 0; i < boid_count; ++i) {
		float new_angle;

		if (boid_mean_angle - boid_arr[i].angle > 0) {
			new_angle = boid_arr[i].angle + time_delta_seconds * BOID_ROTATION_RATE;
			if (new_angle >= 360.0f)
				new_angle = 0.0f;
		}
		else if (boid_mean_angle - boid_arr[i].angle < 0) {
			new_angle = boid_arr[i].angle - time_delta_seconds * BOID_ROTATION_RATE;
			if (new_angle < 0.0f)
				new_angle = 0.0f;
		}
		else {
			new_angle = boid_arr[i].angle;
		}

		boid_arr[i].angle = new_angle;
	}
}

static float boid_calculate_mean_angle(void)
{
	float sum = 0;
	for (size_t i = 0; i < boid_count; ++i)
		sum += boid_arr[i].angle;
	return sum / (float)boid_count;
}

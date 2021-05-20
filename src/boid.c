// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <math.h>
#include <time.h>

#include "boid.h"
#include "screen.h"

// Each boid is represented by a square
#define BOID_SIDE_LENGTH	32.0f	// Side length of the square containing the boid
#define BOID_ROTATED_OFFSET	0.7071f	// Ratio of side length to co-axial radius of a square rotated at 45 degrees
#define BOID_ROTATION_RATE	90.0f	// Boid rotation rate in degrees per second

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)

float boid_w = BOID_SIDE_LENGTH;
float boid_h = BOID_SIDE_LENGTH;

static void boid_align(boid_t boids[], size_t boid_count, clock_t time_delta);
static float boid_calculate_mean_angle(boid_t boids[], size_t boid_count);

void boid_update(boid_t boids[], size_t boid_count, clock_t time_delta)
{
	float boid_wrap_offset_w = (float)boid_w * BOID_ROTATED_OFFSET;
	float boid_wrap_offset_h = (float)boid_h * BOID_ROTATED_OFFSET;

	boid_align(boids, boid_count, time_delta);

	for (size_t i = 0; i < boid_count; ++i) {
		boid_t *boid = &boids[i];

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

static void boid_align(boid_t boids[], size_t boid_count, clock_t time_delta)
{
	float time_delta_seconds = (float)time_delta / (float)CLOCKS_PER_SEC;
	float boid_mean_angle = boid_calculate_mean_angle(boids, boid_count);

	for (size_t i = 0; i < boid_count; ++i) {
		float new_angle;

		if (boid_mean_angle - boids[i].angle > 0) {
			new_angle = boids[i].angle + time_delta_seconds * BOID_ROTATION_RATE;
			if (new_angle >= 360.0f)
				new_angle = 0.0f;
		}
		else if (boid_mean_angle - boids[i].angle < 0) {
			new_angle = boids[i].angle - time_delta_seconds * BOID_ROTATION_RATE;
			if (new_angle < 0.0f)
				new_angle = 0.0f;
		}
		else {
			new_angle = boids[i].angle;
		}

		boids[i].angle = new_angle;
	}
}

static float boid_calculate_mean_angle(boid_t boids[], size_t boid_count)
{
	float sum = 0;
	for (size_t i = 0; i < boid_count; ++i)
		sum += boids[i].angle;
	return sum / (float)boid_count;
}

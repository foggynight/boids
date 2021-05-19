// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <math.h>
#include <stddef.h>
#include <stdlib.h>

#include "boid.h"
#include "screen.h"

// Each boid is represented by a square
#define BOID_SIDE_LENGTH	32
#define BOID_ROTATED_OFFSET	0.7071f

#define BOID_ROTATION_RATE	1	// Number of degrees the boids will rotate per second

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)

int boid_w = BOID_SIDE_LENGTH;
int boid_h = BOID_SIDE_LENGTH;

static void boid_align(boid_t boids[], size_t boid_count);
static int boid_calculate_mean_angle(boid_t boids[], size_t boid_count);

void boid_update(boid_t boids[], size_t boid_count)
{
	float boid_wrap_offset_w = (float)boid_w * BOID_ROTATED_OFFSET;
	float boid_wrap_offset_h = (float)boid_h * BOID_ROTATED_OFFSET;

	boid_align(boids, boid_count);

	for (size_t i = 0; i < boid_count; ++i) {
		boid_t *boid = boids + i;

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

static void boid_align(boid_t boids[], size_t boid_count)
{
	int boid_mean_angle = calculate_boid_mean_angle(boids, boid_count);

	for (size_t i = 0; i < boid_count; ++i) {
		int new_angle;
		if (boid_mean_angle - boids[i].angle > 0) {

		}
		if (boid_mean_angle - boids[i].angle > 0) {

		}
		else {
			new_angle = boids[i].angle;
		}
	}
}

static int boid_calculate_mean_angle(boid_t boids[], size_t boid_count)
{
	int sum = 0;
	for (size_t i = 0; i < boid_count; ++i)
		sum += boids[i].angle;
	return sum / boid_count;
}

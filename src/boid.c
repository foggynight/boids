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

#define TIME_CONSTANT	(1000 / 144)	// Currently same as SLEEP_TIME -- Unsure how I want to handle this

#define deg_to_rad(X)	((double)(X) * M_PI / 180.0)
#define rad_to_deg(X)	((double)(X) * 180.0 / M_PI)

int boid_w = BOID_SIDE_LENGTH;
int boid_h = BOID_SIDE_LENGTH;

typedef struct vector2 {
	float x;
	float y;
} vector2_t;

static vector2_t direction_vectors[MAX_BOID_COUNT];

static void boid_alignment(boid_t boids[], size_t boid_count);
static void boid_calculate_direction_vector(boid_t *boid, vector2_t *direction_vector);

void boid_update(boid_t boids[], size_t boid_count)
{
	boid_alignment(boids, boid_count);

	float boid_wrap_offset_w = (float)boid_w * BOID_ROTATED_OFFSET;
	float boid_wrap_offset_h = (float)boid_h * BOID_ROTATED_OFFSET;

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

static void boid_alignment(boid_t boids[], size_t boid_count)
{
	for (size_t i = 0; i < boid_count; ++i) {
		boid_calculate_direction_vector(&boids[i], &direction_vectors[i]);
	}

	// Align boid angles using direction_vectors
}

static void boid_calculate_direction_vector(boid_t *boid, vector2_t *direction_vector)
{
	direction_vector->x = (float)cos(deg_to_rad(boid->angle));
	direction_vector->y = (float)sin(deg_to_rad(boid->angle));
}

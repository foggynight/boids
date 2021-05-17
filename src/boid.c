// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <math.h>
#include <stddef.h>

#include "boid.h"
#include "screen.h"

#define ROTATED_SQUARE_OFFSET	0.7071f

#define deg_to_rad(X)	(double)((X) * M_PI / 180.0)
#define rad_to_deg(X)	(double)((X) * 180.0 / M_PI)

void boid_update(boid_t boids[], size_t boid_count)
{
	for (size_t i = 0; i < boid_count; ++i) {
		boid_t *boid = boids + i;

		boid->x += boid->velocity * (float)cos(deg_to_rad(boid->angle));
		boid->y += boid->velocity * (float)sin(deg_to_rad(boid->angle));

		if (boid->x < -boid_sprite_w * ROTATED_SQUARE_OFFSET)
			boid->x = (WIN_WIDTH-1) + boid_sprite_w * ROTATED_SQUARE_OFFSET;
		else if (boid->x >= WIN_WIDTH + boid_sprite_w * ROTATED_SQUARE_OFFSET)
			boid->x = -boid_sprite_w * ROTATED_SQUARE_OFFSET;

		if (boid->y < -boid_sprite_h * ROTATED_SQUARE_OFFSET)
			boid->y = (WIN_HEIGHT-1) + boid_sprite_h * ROTATED_SQUARE_OFFSET;
		else if (boid->y >= WIN_HEIGHT + boid_sprite_h * ROTATED_SQUARE_OFFSET)
			boid->y = -boid_sprite_h * ROTATED_SQUARE_OFFSET;
	}
}

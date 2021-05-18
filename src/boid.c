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
	float boid_wrap_offset_w = (float)boid_sprite_w * ROTATED_SQUARE_OFFSET;
	float boid_wrap_offset_h = (float)boid_sprite_h * ROTATED_SQUARE_OFFSET;

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

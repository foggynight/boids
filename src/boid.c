// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <math.h>
#include <stddef.h>

#include "boid.h"

#define deg_to_rad(X)	(double)((X) * M_PI / 180.0)
#define rad_to_deg(X)	(double)((X) * 180.0 / M_PI)

void boid_update(boid_t boids[], size_t boid_count)
{
	for (size_t i = 0; i < boid_count; ++i) {
		boids[i].x += boids[i].velocity * (float)cos(deg_to_rad(boids[i].angle));
		boids[i].y -= boids[i].velocity * (float)sin(deg_to_rad(boids[i].angle));
	}
}

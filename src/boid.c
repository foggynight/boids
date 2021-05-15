// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <math.h>
#include <stddef.h>

#include "boid.h"

void boid_update(boid_t boids[], size_t boid_count)
{
	for (size_t i = 0; i < boid_count; ++i) {
		boids[i].x += boids[i].velocity * (float)cos(boids[i].angle);
		boids[i].y += boids[i].velocity * (float)sin(boids[i].angle);
	}
}

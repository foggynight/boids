// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOID_H
#define BOID_H

#include <stddef.h>

typedef struct boid {
	float x;	// x position
	float y;	// y position
	int angle;	// Angle relative to the right facing x-axis
	float velocity;	// Forward velocity
} boid_t;

void boid_update(boid_t boids[], size_t boid_count);

#endif	// BOID_H

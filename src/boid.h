// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOID_H
#define BOID_H

#include <stddef.h>

#define MAX_BOID_COUNT	16

typedef struct boid {
	float x;	// x position
	float y;	// y position
	float velocity;	// Forward velocity
	int angle;	// Angle in degrees -- Relative to the right facing x-axis with a clockwise rotation
} boid_t;

extern int boid_w, boid_h;

void boid_update(boid_t boids[], size_t boid_count);

#endif	// BOID_H

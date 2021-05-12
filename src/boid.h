// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOID_H
#define BOID_H

typedef struct boid {
	int x;	// x position
	int y;	// y position
	int angle;	// Angle relative to the right facing x-axis
	float velocity;	// Forward velocity
} boid_t;

#endif	// BOID_H

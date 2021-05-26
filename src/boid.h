// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOID_H
#define BOID_H

#include <time.h>

#define MAX_BOID_COUNT	16	// Max number of boids able to be simulated -- Size of boid array

typedef struct boid {
	float x;	// x position
	float y;	// y position
	float angle;	// Angle in degrees -- Relative to the right facing x-axis with a clockwise rotation
	float velocity;	// Forward velocity
} boid_t;

extern boid_t boid_arr[MAX_BOID_COUNT];	// Array containing boids to be simulated
extern size_t boid_count;	// Number of boids currently in boid_arr

extern float boid_w, boid_h;	// Width and height of a boid

/**
 * Update all boids in the boid array.
 *
 * @param time_delta	Number of clock cycles since previous update
 **/
void boid_update(clock_t time_delta);

#endif	// BOID_H

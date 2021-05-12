// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOYD_H
#define BOYD_H

typedef struct boyd {
	int x;	// x position
	int y;	// y position
	int angle;	// Angle relative to the right facing x-axis
	float velocity;	// Forward velocity
} boyd_t;

#endif	// BOYD_H

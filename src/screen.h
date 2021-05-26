// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SCREEN_H
#define SCREEN_H

#include <stddef.h>

#include "boid.h"

#define WIN_WIDTH	1280	// Width of the window
#define WIN_HEIGHT	720	// Height of the window

/**
 * Initialize the screen and associated modules.
 **/
void screen_init(void);

/**
 * Destroy the screen and associated modules.
 **/
void screen_destroy(void);

/**
 * Update the screen -- Draw all the boids.
 **/
void screen_update(void);

#endif	// SCREEN_H

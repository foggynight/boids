// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SCREEN_H
#define SCREEN_H

#include <stddef.h>

#define WIN_WIDTH	1920	// Width of the window
#define WIN_HEIGHT	1080	// Height of the window

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

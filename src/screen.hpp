// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SCREEN_HPP
#define SCREEN_HPP

#include <vector>

#include "Boid.hpp"

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
void screen_update(std::vector<Boid> &boid_vec);

#endif	// SCREEN_HPP

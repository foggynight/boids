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
void screen_init();

/**
 * Destroy the screen and associated modules.
 **/
void screen_destroy();

/**
 * Update the screen -- Draw all the boids.
 *
 * @param boid_vec	Vector to source boid information from
 **/
void screen_update(const std::vector<Boid>& boid_vec);

#endif	// SCREEN_HPP

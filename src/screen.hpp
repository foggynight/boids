// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SCREEN_HPP
#define SCREEN_HPP

#include <vector>

#include "Boid.hpp"
#include "Entity.hpp"

#define WIN_WIDTH	1280	// Width of the window
#define WIN_HEIGHT	720	// Height of the window

namespace screen {

/**
 * Initialize the screen and associated modules.
 **/
void init();

/**
 * Destroy the screen and associated modules.
 **/
void destroy();

/**
 * Clear the screen.
 **/
void clear();

/**
 * Present what's been drawn to the screen.
 **/
void present();

/**
 * Draw all the boids.
 *
 * @param boid_vec	Vector to source boids from
 **/
void draw_boids(const std::vector<Boid>& boid_vec);

/**
 * Draw a line connecting the positions of two entities.
 *
 * @param reference	Entity at which to start the line
 * @param target	Entity at which to end the line
 **/
void draw_line_between(const Entity& reference, const Entity& target);

}

#endif	// SCREEN_HPP

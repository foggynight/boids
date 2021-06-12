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
 * Initialize the screen.
 **/
void init();

/**
 * Destroy the screen.
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
 * @param boid_vec	vector to source boids from
 **/
void draw_boids(std::vector<Boid>& boid_vec);

/**
 * Draw the field of view of an entity.
 *
 * @param entity	Entity to derive FOV from
 **/
void draw_fov(Entity& entity);

/**
 * Draw a line connecting the positions of two entities.
 *
 * @param reference	Entity to start line at
 * @param target	Entity to end line at
 **/
void draw_line_between(const Entity& reference, const Entity& target);

}

#endif	// SCREEN_HPP

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SETUP_HPP
#define SETUP_HPP

#include <string>
#include <vector>

#include "Boid.hpp"

/**
 * Setup the boid vector by reading the initial parameters of the boids from a
 * CSV file, using those parameters to construct the boids in the vector.
 *
 * @param boid_vec	vector to fill with boids
 * @param file_path	Path to file containing initial boid parameters
 *
 * @param false if opening the setup file fails, otherwise true
 **/
bool setup_boid_vec(std::vector<Boid>& boid_vec, const std::string& setup_file);

#endif	// SETUP_HPP

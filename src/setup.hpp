// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SETUP_HPP
#define SETUP_HPP

#include <cstdio>
#include <vector>

#include "Boid.hpp"

/**
 * Setup the boid vector by reading the initial parameters of the boids
 * from a CSV file, using those parameters to construct the boids in the
 * vector.
 *
 * @param boid_vec	Vector to fill with boids
 * @param setup_file	CSV file containing initial boid parameters
 **/
void setup_boid_vec(std::vector<Boid> &boid_vec, FILE *setup_file);

#endif	// SETUP_HPP

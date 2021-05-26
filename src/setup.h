// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SETUP_H
#define SETUP_H

#include <stdio.h>

#include "boid.h"

/**
 * Setup the boids by reading their initial parameters from a CSV file
 * and filling in the members of the boids in the boid array.
 *
 * @param setup_file	File containing initial boid parameters
 * @param boids	Boid array
 * @param max_boid_count	Maximum number of boids to read from the file
 *
 * @return Number of boids read from the file
 **/
size_t setup_boids(FILE *setup_file, boid_t boids[], size_t max_boid_count);

#endif	// SETUP_H

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SETUP_H
#define SETUP_H

#include <stdio.h>

/**
 * Setup the boids by reading their initial parameters from a CSV file
 * and filling in the members of the boids in the boid array.
 *
 * @param setup_file	File containing initial boid parameters in CSV format
 *
 * @sideeffect Fills in the members of the boids in the boid_arr global
 * @sideeffect Sets the boid_count global to the number of lines read
 **/
void setup_boid_arr(FILE *setup_file);

#endif	// SETUP_H

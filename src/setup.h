// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SETUP_H
#define SETUP_H

#include <stdio.h>

#include "boid.h"

size_t setup_boids(FILE *setup_file, boid_t boids[], size_t max_boid_count);

#endif	// SETUP_H

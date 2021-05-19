// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "boid.h"
#include "setup.h"

#define MAX_LINE_LENGTH	128

size_t setup_boids(FILE *setup_file, boid_t boids[], size_t max_boid_count)
{
	size_t boid_count;
	char line[MAX_LINE_LENGTH+1];

	for (boid_count = 0; boid_count < max_boid_count; ++boid_count) {
		if (fscanf(setup_file, "%s", line) == EOF)
			break;

		boids[boid_count].x = atof(strtok(line, ","));
		boids[boid_count].y = atof(strtok(NULL, ","));
		boids[boid_count].angle = atof(strtok(NULL, ","));
		boids[boid_count].velocity = atof(strtok(NULL, "\n"));
	}

	return boid_count;
}

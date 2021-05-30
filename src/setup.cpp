// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>

#include "Boid.hpp"
#include "setup.hpp"

#define MAX_LINE_LENGTH	128	// Max length of a line of boid parameters

void setup_boid_vec(std::vector<Boid> &boid_vec, FILE *setup_file)
{
	char line[MAX_LINE_LENGTH+1];
	while (fscanf(setup_file, "%s", line) != EOF) {
		const float w = atof(strtok(line, ","));
		const float h = atof(strtok(NULL, ","));
		const float x = atof(strtok(NULL, ","));
		const float y = atof(strtok(NULL, ","));
		const float angle = atof(strtok(NULL, ","));
		const float velocity = atof(strtok(NULL, "\n"));
		boid_vec.emplace_back(w, h, x, y, angle, velocity);
	}
}

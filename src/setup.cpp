// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cstdlib>
#include <cstring>
#include <fstream>
#include <string>
#include <vector>

#include "Boid.hpp"
#include "Vec2.hpp"
#include "setup.hpp"

#define MAX_LINE_LENGTH	128	// Max length of a line of boid parameters

bool setup_boid_vec(std::vector<Boid>& boid_vec, const std::string& file_path)
{
	std::ifstream setup_file;
	setup_file.open(file_path);
	if (!setup_file.is_open())
		return false;

	char line[MAX_LINE_LENGTH+1];
	while (!setup_file.getline(line, MAX_LINE_LENGTH+1).eof()) {
		const float x = atof(strtok(line, ","));
		const float y = atof(strtok(NULL, ","));
		const float angle = atof(strtok(NULL, ","));
		const float velocity = atof(strtok(NULL, "\n"));
		boid_vec.emplace_back(Vec2(x, y), angle, velocity);
	}

	setup_file.close();
	return true;
}

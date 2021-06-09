// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <cstddef>
#include <vector>

#include "Boid.hpp"

extern "C" {
#include "util.h"
}

void Boid::get_neighbors(std::vector<Boid>& boid_vec, std::vector<Boid *>& neighbor_vec)
{
	for (auto& boid : boid_vec)
		if (&boid != this && in_fov(boid))
			neighbor_vec.push_back(&boid);
}

float Boid::alignment(const std::vector<Boid *>& neighbor_vec)
{
	assert(neighbor_vec.size() > 0);

	float mean_angle = 0.0f;
	for (const auto& neighbor : neighbor_vec)
		mean_angle += neighbor->angle;
	mean_angle /= (float)neighbor_vec.size();

	return mean_angle;
}

float Boid::cohesion(const std::vector<Boid *>& neighbor_vec)
{
	assert(neighbor_vec.size() > 0);

	float mean_x = 0.0f, mean_y = 0.0f;
	for (const auto& neighbor : neighbor_vec) {
		mean_x += neighbor->x;
		mean_y += neighbor->y;
	}
	const std::size_t neighbor_count = neighbor_vec.size();
	mean_x /= (float)neighbor_count;
	mean_y /= (float)neighbor_count;

	return get_vec2_angle(mean_x - x, mean_y - y);
}

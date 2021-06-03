// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <vector>

#include "Boid.hpp"

void Boid::get_neighbors(std::vector<Boid>& boid_vec, std::vector<Boid *>& neighbor_vec)
{
	for (auto& boid : boid_vec)
		if (&boid != this && in_fov(boid))
			neighbor_vec.push_back(&boid);
}

void Boid::align_with_neighbors(const std::vector<Boid *>& neighbor_vec, int delta_time_us)
{
	assert(neighbor_vec.size() > 0);

	float mean_angle = 0;
	for (const auto& neighbor : neighbor_vec)
		mean_angle += neighbor->angle;
	mean_angle /= neighbor_vec.size();

	rotate_towards(mean_angle, delta_time_us);
}

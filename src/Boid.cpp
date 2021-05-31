// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <vector>

#include "Boid.hpp"

void Boid::get_neighbors(std::vector<Boid>& boid_vec, std::vector<Boid *>& neighbor_vec)
{
	for (auto& boid : boid_vec)
		if (&boid != this && in_fov(boid))
			neighbor_vec.push_back(&boid);
}
